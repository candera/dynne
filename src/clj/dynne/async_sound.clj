(ns dynne.async-sound
  "Functions for manipulating a sound whose amplitude representation
  is channels shipping around arrays of doubles."
  (:require [clojure.core.async :as async :refer (close! go go-loop chan
                                                         >! <! alt! alt!!
                                                         >!! <!!)]
            [clojure.java.io :as io]
            [hiphip.double :as dbl]
            [incanter.core :as incanter]
            [incanter.charts :as charts]
            [primitive-math :as p])
  (:import [java.nio ByteBuffer]
           [java.util.concurrent LinkedBlockingQueue]
           [javax.sound.sampled
            AudioFileFormat$Type
            AudioFormat
            AudioFormat$Encoding
            AudioInputStream
            AudioSystem]))

;;; core.async stuff

;; The size of the windowed buffers that async channels will use.
;; TODO: Pick a good default, and maybe add knobs for changing it in
;; various stages of the pipeline.
(def ^:dynamic *processing-options* {:buffer-size 10})

(defn make-chan
  "Makes a new core.async channel with the default configuration."
  []
  (chan (:buffer-size *processing-options*)))

;;; Abstraction

(defprotocol Sound
  "Represents a sound as a channel delivering vectors of Java double arrays."
  (channels [this] "Returns the number of channels in the sound.")
  (duration [this] "Returns the duration of the sound in seconds.")
  (chunks [this sample-rate err-chan] "Returns a channel that delivers
  chunks. A chunk is a vector of Java double arrays - one per audio
  channel - populated with the data for one audio channel of sound.
  Also accepts a channel where any errors should be put."))

;;; Helpers

(defn take-chunk
  "Given an channel producing chunks and an error channel, produce the
  next chunk from the chunk channel, unless an error is available on
  the error channel, in which case, throw it. Return nil if the chunks
  channel has closed."
  [chunks errors]
  (alt!!
   errors ([error] (throw error))
   chunks ([chunk] chunk)))

(defn chunk-seq
  "Given a sound and a sample rate, return a regular Clojure seq of
  the chunks."
  [chunks errors]
  (when-let [chunk (take-chunk chunks errors)]
    (lazy-seq (cons chunk (chunk-seq chunks errors)))))

;;; Sound construction

(defmacro defsound
  "Expands to define a function `name` that accepts arguments `args`
  returns a sound with `duration`, `channels` whose samples are
  determined by `expr`. Inside expr, the sample rate, the total number
  of samples, the current sample index, and the current channel number
  will be bound to the four symbols in `bindings`."
  [name
   duration-param
   channels-param
   docstring
   args
   [sample-rate num-samples index c]
   expr]
  `(defn ~name
     ~docstring
     ~(vec (concat [duration-param
                    channels-param]
                   args))
     (let [duration# (double ~duration-param)
           chans# (double ~channels-param)]
       (reify Sound
         (channels [this#] ~channels-param)
         (duration [this#] duration#)
         (chunks [this# ~sample-rate err-chan#]
           (let [chunk-size#  10000
                 ~num-samples (long (* duration# ~sample-rate))
                 num-chunks#  (-> ~num-samples (/ chunk-size#) Math/ceil long)
                 out-chan#    (make-chan)]
             (go
              (try
                (doseq [chunk-num# (range (dec num-chunks#))]
                  (let [base-index# (p/* (long chunk-num#) chunk-size#)]
                    (>! out-chan#
                        (for [~c (range chans#)]
                          (dbl/amake [i# chunk-size#]
                                     (let [~index (p/+ i# base-index#)]
                                       ~expr))))))
                (let [chunks-so-far# (p/- num-chunks# 1)
                      samples-so-far# (p/* chunk-size# chunks-so-far#)
                      samples-remaining# (p/- ~num-samples samples-so-far#)]
                  (>! out-chan#
                      (for [~c (range chans#)]
                        (dbl/amake [i# samples-remaining#]
                                   (let [~index (p/+ i# (p/* (p/- num-chunks# 1) chunk-size#))]
                                     ~expr)))))
                (catch Throwable t
                  (>! err-chan# t))
                (finally
                  (close! out-chan#))))
             out-chan#))))))

(defsound constant duration chans
  "Returns a sound of `duration` that has `chans` channels, each of
  which is constant at `x`."
  [x]
  [sample-rate num-samples i c]
  x)

(defn silence
  "Returns a sound of `duration` with `chans` channels of silence."
  [dur chans]
  (constant dur chans 0.0))

;; TODO: It would be nice if we had a way to indicate local bindings
;; that we want to be in effect outside the amake so we don't have all
;; these stupid calls to double inside the inner loop.
(defsound linear duration chans
  "Returns a sound of `duration` that has `chans` channels, each of
  which changes linearly from `start` to `end`."
  [start end]
  [sample-rate num-samples i c]
  (p/+ (double start)
       (p/* (p/- (double end)
                 (double start))
            (p/div (double i)
                   (double num-samples)))))

(defsound fn-sound duration chans
  "Creates a SampledSound `duration` seconds long where the amplitudes
  are produced by `f`, a function of a channel number and a time in
  seconds."
  [f]
  [sample-rate num-samples i c]
  (f c (p/div (double i) (double sample-rate))))

(defn sinusoid
  "Returns a single-channel sound of `duration` and `frequency`"
  [^double duration ^double frequency]
  (fn-sound duration 1 (fn sinusoid-fn [^long c ^double t]
                      (Math/sin (p/* t frequency 2.0 Math/PI)))))

(defn square-wave
  "Produces a single-channel sound that toggles between 1.0 and -1.0
  at frequency `freq`."
  [^double duration ^double frequency]
  (fn-sound duration 1 (fn square-wave-fn [^long c ^double t]
                         (let [x (-> t (p/* frequency 2.0) long)]
                           (if (even? x) 1.0 -1.0)))))

(defn- to-double-arrays
  "Return a seq of arrays of doubles that decode the values in buf."
  [^bytes buf ^long bytes-read ^long bytes-per-sample ^long chans]
  (let [samples-read (/ bytes-read bytes-per-sample chans)
        bb           (ByteBuffer/allocate bytes-read)
        arrs         (repeatedly chans #(double-array samples-read))]
    (.put bb buf 0 bytes-read)
    (.position bb 0)
    (dotimes [n samples-read]
      (doseq [arr arrs]
        ;; TODO: We're hardcoded to .getShort here, but the
        ;; bytes-per-sample is a parameter. Should probably have
        ;; something that knows how to read from a ByteBuffer given a
        ;; number of bits.
        (dbl/aset arr n (p/div (double (.getShort bb)) 32768.0))))
    arrs))

;; TODO: Figure out how to clean this up in the face of errors.
(defn- deliver-chunks
  "Deliver chunks from an AudioInputStream onto `out`. Delivers errors
  on `errors`."
  [^AudioInputStream ais chans bytes-per-sample chunk-size out errors]
  (let [buf (byte-array (p/* chunk-size ^long chans ^long bytes-per-sample))]
    (async/thread
     (try
       (loop [bytes-read (.read ^AudioInputStream ais ^bytes buf)]
         (when (p/< 0 bytes-read)
           (>!! out (to-double-arrays buf (long bytes-read) bytes-per-sample chans))
           (recur (.read ^AudioInputStream ais ^bytes buf))))
       (catch Throwable t
         (>!! errors t))
       (finally
         (async/close! out))))))

(defn- read-duration
  "Given a path to a .wav or .mp3 file, return the duration in
  seconds."
  [path]
  (let [file                 (io/file path)
        base-file-format     (AudioSystem/getAudioFileFormat file)
        base-file-properties (.properties base-file-format)
        base-file-duration   (get base-file-properties "duration")]
    (if base-file-duration
      (/ base-file-duration 1000000.0)
      (let [in (AudioSystem/getAudioInputStream file)
            base-format (.getFormat in)
            frame-length (.getFrameLength in)
            frames-per-second (.getSampleRate base-format)]
        (.close in)
        (/ frame-length (double frames-per-second))))))

(defn read-sound
  "Given a path to a .wav or .mp3 file, return a SampledSound instance
  over it."
  [path]
  (let [file                 (io/file path)
        base-file-format     (-> file  AudioSystem/getAudioFileFormat .getFormat)
        base-file-properties (.properties base-file-format)
        dur                  (read-duration path)
        chans                (.getChannels base-file-format)
        file-sample-rate     (.getSampleRate base-file-format)
        file-encoding        (.getEncoding base-file-format)]
    (reify Sound
      (channels [this] chans)
      (duration [this] dur)
      (chunks [this sample-rate err-chan]
        (let [out-chan         (make-chan)
              bits-per-sample  16
              bytes-per-sample (-> bits-per-sample (/ 8) long)
              in               (AudioSystem/getAudioInputStream file)
              decoded          (if (= AudioFormat$Encoding/PCM_SIGNED file-encoding)
                                 in
                                 (AudioSystem/getAudioInputStream
                                  (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                                                file-sample-rate
                                                bits-per-sample
                                                chans
                                                (* bytes-per-sample chans)
                                                file-sample-rate
                                                true)
                                  ^AudioInputStream in))
              resampled        (if (= sample-rate file-sample-rate)
                                 decoded
                                 (AudioSystem/getAudioInputStream
                                  (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                                                sample-rate
                                                bits-per-sample
                                                chans
                                                (* bytes-per-sample chans)
                                                sample-rate
                                                true)
                                  ^AudioInputStream decoded))]
          (deliver-chunks resampled chans bytes-per-sample 10000 out-chan err-chan)
          out-chan)))))

;;; Sound manipulation

(defn peak
  "Returns the maximum absolute amplitude of `s` when sampled at
  `sample-rate`. If provided, will return immediately on finding a
  value above `limit`."
  ([s sample-rate] (peak s sample-rate Double/MAX_VALUE))
  ([s sample-rate limit]
     (let [errors (make-chan)
           chunks (chunks s sample-rate errors)]
       (loop [chunk (take-chunk chunks errors)
              max-amplitude Double/MIN_VALUE]
         (cond
          ;; Short-circuit if we hit `limit`
          (< limit max-amplitude) max-amplitude

          ;; Chunks has been consumed
          (not chunk) max-amplitude

          :else
          (recur (take-chunk chunks errors)
                 (double (apply max
                                (map (fn [arr]
                                       (dbl/areduce [e arr]
                                                    m max-amplitude
                                                    (max m (Math/abs e))))
                                     chunk)))))))))

;;; Sound operations

;; An operation takes one or more sounds and returns a new sound

(defn append
  "Concatenates any number of sounds together."
  [& inputs]
  {:pre [(apply = (map channels inputs))]}
  (reify Sound
    (duration [this] (->> inputs (map duration) (reduce +)))
    (channels [this] (channels (first inputs)))
    (chunks [this sample-rate errors]
      (let [out (make-chan)]
        (go
         (try
           (loop [[src & more :as srcs]
                  (map #(chunks % sample-rate errors) inputs)]
             (when src
               (if-let [chunk (take-chunk src errors)]
                 (do (>! out chunk)
                     (recur srcs))
                 (recur more))))
           (catch Throwable t
             (>! errors t))
           (finally (async/close! out)))
         (async/close! out))
        out))))

(defn- dbl-asub
  "Returns the part of `arr` whose indices fall in [`start` `end`)."
  [arr ^long start ^long end]
  (dbl/amake [i (p/- end start)]
             (dbl/aget arr (p/+ i start))))

(defn- drop-samples
  "Drops `n` samples from `chunks`."
  [^long n chunks]
  (cond
   (zero? n) chunks

   (< n (dbl/alength (ffirst chunks)))
   (lazy-seq
    (cons (map #(dbl-asub % n (dbl/alength %)) (first chunks))
          (rest chunks)))

   (seq chunks)
   (recur (- n (dbl/alength (ffirst chunks))) (rest chunks))))

(defn- take-samples
  "Returns chunks from `chunks` until `n` samples have been returned."
  [^long n chunks]
  (cond
   (not (seq chunks)) nil

   (not (pos? n)) nil

   (< n (dbl/alength (ffirst chunks)))
   [(map #(dbl-asub % 0 n) (first chunks))]

   :else
   (lazy-seq
    (cons (first chunks)
          (take-samples (- n (dbl/alength (ffirst chunks)))
                        (rest chunks))))))

(defn pipef
  "Like core.async/pipe, but transforms values by `f` before copying
  them."
  [from to f]
  (go-loop []
           (let [v (<! from)]
             (if (nil? v)
               (async/close! to)
               (do (>! to (f v))
                   (recur)))))
  to)

(defn multiplex
  "Takes a single-channel sound `s` and returns an `n`-channel sound
  whose channels are all identical to channel 0 of `s`."
  [s ^long n]
  {:pre [(== 1 (channels s))]}
  (if (== 1 n)
    s
    (reify Sound
      (channels [this] n)
      (chunks [this sample-rate errors]
        (pipef (chunks s sample-rate errors)
               (make-chan)
               (fn [[arr]] (repeat n arr)))))))

(defn trim
  "Truncates `s` to the region between `start` and `end`. If `end` is
  beyond the end of the sound, just trim to the end."
  [s ^double start ^double end]
  {:pre [(<= 0 start (duration s))
         (<= start end)]}
  (let [end* (min (duration s) end)
        dur  (- end* start)]
    (reify Sound
      (duration [this] dur)
      (channels [this] (channels s))
      (chunks [this sample-rate errors]
        (let [samples-to-drop (-> start (* sample-rate) long)
              samples-to-take (-> dur (* sample-rate) long)
              src-chunks (chunks s sample-rate errors)
              out-chan (make-chan)]
          ;; TODO: This doesn't work with go - figure out why and switch
          (async/thread
           (try
             (loop [samples-to-drop samples-to-drop
                    samples-to-take samples-to-take]
               (if-let [chunk (take-chunk src-chunks errors)]
                 (let [chunk-length (dbl/alength (first chunk))]
                   (cond

                    ;; First we maybe drop a bunch of samples
                    (pos? samples-to-drop)
                    (if (< samples-to-drop chunk-length)
                      ;; It's possible we're asked to take fewer
                      ;; than remain in the chunk
                      (let [partial-chunk-samples (min samples-to-take
                                                       (- chunk-length samples-to-drop))]
                        (>!! out-chan (map #(dbl-asub %
                                                      samples-to-drop
                                                      (+ partial-chunk-samples samples-to-drop))
                                           chunk))
                        (recur 0 (- samples-to-take partial-chunk-samples)))
                      (recur (- samples-to-drop chunk-length)
                             samples-to-take))

                    ;; Then we maybe take some
                    (pos? samples-to-take)
                    (do (if (< samples-to-take chunk-length)
                          (>!! out-chan (map #(dbl-asub % 0 samples-to-take) chunk))
                          (>!! out-chan chunk))
                        (recur samples-to-drop
                               (- samples-to-take chunk-length)))))))
             (catch Throwable t
               (>!! errors t))
             (finally
               (close! out-chan))))
          out-chan)))))

(defn- combine-chunks
  "Returns a channel that will deliver a sequence of chunks whose
  contents are corresponding elements of `input1` and `input2`, each a
  channel delivering chunks, combined by calling `f` on them. `f`
  should be a function of the number of samples in the chunk to be
  produced, the first chunk, the offset in that chunk at which to
  start, the second chunk, and the offset in that chunk at which to
  start. If no offsets are provided, defaults to zero. If either
  channel runs out of data, delivers the remaining data in the other
  channel without applying f. `on-eof` specifies what to do when
  reaching the end of one of the inputs, and must be `:terminate` or
  `:continue`. If :continue, provides the remainder of the other
  input. If :terminate, closes the output."
  ([f input1 input2 errors on-eof] (combine-chunks f input1 0 input2 0 errors on-eof))
  ([f input1 offset1 input2 offset2 errors on-eof]
     (let [out (make-chan)]
       (go
        (try
          (loop [chunk1 (take-chunk input1 errors)
                 offset1 offset1
                 chunk2 (take-chunk input2 errors)
                 offset2 offset2]
            (cond
             (and chunk1 chunk2)
             (let [len1       (dbl/alength (first chunk1))
                   len2       (dbl/alength (first chunk2))
                   samples    (min (- len1 offset1) (- len2 offset2))
                   consumed1? (= len1 (+ samples offset1))
                   consumed2? (= len2 (+ samples offset2))]
               (>! out (f samples chunk1 offset1 chunk2 offset2))
               (recur (if consumed1? (take-chunk input1 errors) chunk1)
                      (if consumed1? 0 (+ offset1 samples))
                      (if consumed2? (take-chunk input2 errors) chunk2)
                      (if consumed2? 0 (+ offset2 samples))))

             (and (= on-eof :continue) chunk1 (not chunk2))
             (do
               (>! out (map #(dbl-asub % offset1 (dbl/alength %)) chunk1))
               (recur (take-chunk input1 errors) 0 nil 0))

             (and (= on-eof :continue) (not chunk1) chunk2)
             (do
               (>! out (map #(dbl-asub % offset2 (dbl/alength %)) chunk2))
               (recur nil 0 (take-chunk input2 errors) 0))))
          (catch Throwable t
            (>! errors t))
          (finally
            (close! out))))
       out)))

(defn- add-chunk
  "Adds two chunks together, starting at the corresponding `offset`
  inside each one."
  [samples chunk1 offset1 chunk2 offset2]
  (let [o1 (long offset1)
        o2 (long offset2)]
    (map #(dbl/amake [i samples]
                     (p/+ (dbl/aget %1 (p/+ i o1))
                          (dbl/aget %2 (p/+ i o2))))
         chunk1
         chunk2)))

(defn mix
  "Mixes sounds `s1` and `s2` together."
  [s1 s2]
  {:pre [(= (channels s1) (channels s2))]}
  (let [d1 (duration s1)
        d2 (duration s2)]
    (reify Sound
      (duration [this] (max d1 d2))
      (channels [this] (channels s1))
      (chunks [this sample-rate errors]
        (combine-chunks add-chunk
                        (chunks s1 sample-rate errors)
                        (chunks s2 sample-rate errors)
                        errors
                        :continue)))))

(defn gain
  "Multiplies the amplitudes of `s` by `g`."
  [s ^double g]
  (reify Sound
    (duration [this] (duration s))
    (channels [this] (channels s))
    (chunks [this sample-rate errors]
      (let [out (make-chan)
            src (chunks s sample-rate errors)]
        ;; TODO: Make this work with go, if possible. For now, it
        ;; requires the thread macro because go doesn't compose well
        ;; with the type hinting emitted by the primitive math
        ;; library.
        (async/thread
         (try
           (loop []
             (when-let [chunk (take-chunk src errors)]
               (>!! out (map (fn [data]
                              (dbl/amap [x data]
                                        (p/* x g)))
                            chunk))
               (recur)))
           (catch Throwable t (>! errors t))
           (finally (close! out))))
        out))))

(defn multiply-chunk
  "Multiplies the samples in two chunks together, starting at the
  corresponding `offset` inside each one."
  [samples chunk1 offset1 chunk2 offset2]
  (map #(dbl/amake [i samples]
                   (p/* (dbl/aget %1 (p/+ i (long offset1)))
                        (dbl/aget %2 (p/+ i (long offset2)))))
       chunk1
       chunk2))

(defn envelope
  "Multiplies the amplitudes of `s1` and `s2`, trimming the sound to
  the shorter of the two."
  [s1 s2]
  {:pre [(= (channels s1) (channels s2))]}
  (let [dur (min (duration s1) (duration s2))]
    (reify Sound
      (duration [this] dur)
      (channels [this] (channels s1))
      (chunks [this sample-rate errors]
        (combine-chunks multiply-chunk
                        (chunks s1 sample-rate errors)
                        (chunks s2 sample-rate errors)
                        errors
                        :terminate)))))

(defn fade-in
  "Fades `s` linearly from zero at the beginning to full volume at
  `duration`."
  [s ^double fade-duration]
  (let [chans (channels s)]
    (-> (linear fade-duration chans 0 1.0)
        (append (constant (- (duration s) fade-duration) chans 1.0))
        (envelope s))))

(defn fade-out
  "Fades the s to zero for the last `duration`."
  [s ^double fade-duration]
  (let [chans (channels s)]
    (-> (constant (- (duration s) fade-duration) chans 1.0)
        (append (linear fade-duration chans 1.0 0))
        (envelope s))))

(defn segmented-linear
  "Produces a sound with `chans` channels whose amplitudes change
  linearly as described by `spec`. Spec is a sequence of interleaved
  amplitudes and durations. For example the spec

  1.0 30
  0   10
  0   0.5
  1.0

  (written that way on purpose - durations and amplitudes are in columns)
  would produce a sound whose amplitude starts at 1.0, linearly
  changes to 0.0 at time 30, stays at 0 for 10 seconds, then ramps up
  to its final value of 1.0 over 0.5 seconds"
  [chans & spec]
  {:pre [(and (odd? (count spec))
              (< 3 (count spec)))]}
  (->> spec
       (partition 3 2)
       (map (fn [[start duration end]] (linear duration chans start end)))
       (reduce append)))

(defn timeshift
  "Inserts `dur` seconds of silence at the beginning of `s`"
  [s ^double dur]
  (append (silence dur (channels s)) s))

(defn ->stereo
  "Creates a stereo sound. If given one single-channel sound,
  duplicates channel zero on two channels. If given a single stereo
  sound, returns it. If given two single-channel sounds, returns a
  sound with the first sound on channel 0 and the second sound on
  channel 1."
  ([s]
     (case (long (channels s))
       2 s
       1 (reify Sound
           (duration [this] (duration s))
           (channels [this] 2)
           (chunks [this sample-rate errors]
             (async/map< (fn [[chunk]] [chunk chunk])
                         (chunks s sample-rate errors))))
       (throw (ex-info "Can't steroize sound with other than one or two channels"
                       {:reason :cant-stereoize-channels
                        :s      s}))))
  ([l r]
     (when-not (= 1 (channels l) (channels r))
       (throw (ex-info "Can't steroize two sounds unless they are both single-channel"
                       {:reason :cant-stereoize-channels
                        :l-channels (channels l)
                        :r-channels (channels r)})))
     (reify Sound
       (duration [this] (min (duration l) (duration r)))
       (channels [this] 2)
       (chunks [this sample-rate errors]
         (combine-chunks (fn stereo-fn [samples [chunk1] offset1 [chunk2] offset2]
                             [(dbl-asub chunk1 offset1 (+ offset1 samples))
                              (dbl-asub chunk2 offset2 (+ offset2 samples))])
                         (chunks l sample-rate errors)
                         (chunks r sample-rate errors)
                         errors
                         :terminate)))))

(defn pan
  "Takes a two-channel sound and mixes the channels together by
  `amount`, a float on the range [0.0, 1.0]. The ususal use is to take
  a sound with separate left and right channels and combine them so
  each appears closer to stereo center. An `amount` of 0.0 would leave
  both channels unchanged, 0.5 would result in both channels being the
  same (i.e. appearing to be mixed to stereo center), and 1.0 would
  switch the channels."
  [s ^double amount]
  {:pre [(= 2 (channels s))]}
  (let [amount-complement (- 1.0 amount)]
    (reify Sound
      (duration [this] (duration s))
      (channels [this] 2)
      (chunks [this sample-rate errors]
        (async/map< (fn [[arr1 arr2]]
                      [(dbl/amap [e1 arr1
                                  e2 arr2]
                                 (p/+ (p/* e1 amount-complement)
                                      (p/* e2 amount)))
                       (dbl/amap [e1 arr1
                                  e2 arr2]
                                 (p/+ (p/* e1 amount)
                                      (p/* e2 amount-complement)))])
                    (chunks s sample-rate errors))))))

;;; Playback

;; TODO: This is identical to the one in sound.clj. Merge them if we
;; don't get rid of sound.clj
(defmacro shortify
  "Takes a floating-point number f in the range [-1.0, 1.0] and scales
  it to the range of a 16-bit integer. Clamps any overflows."
  [f]
  (let [max-short-as-double (double Short/MAX_VALUE)]
    `(let [clamped# (-> ~f (min 1.0) (max -1.0))]
       (short (p/* ~max-short-as-double clamped#)))))

(defn maybe-put
  "Puts v on ch, but only if v is non-nil."
  [ch v]
  (when v (>!! ch v)))

(defn- sample-provider
  "Delivers byte arrays representing the data to `out`. Returns a
  channel that can be closed to stop the process."
  [s ^long sample-rate out]
  (let [chans (channels s)
        errors (make-chan)
        chunks (chunks s sample-rate errors)
        process (async/chan)]
    (async/thread
     (maybe-put
      process
      (try
        (loop []
          (let [[event val] (async/alt!!
                             process ([v] [(if v :process-msg :process-close) v])
                             chunks ([f] [(if f :chunk :chunk-close) f])
                             errors ([e] [:error e]))]
               (case event
                 ;; Process messages are ignored
                 :process-msg (recur)

                 :process-close nil

                 :chunk
                 (let [chunk-len  (dbl/alength (first val))
                       byte-count (p/* chans 2 chunk-len)
                       bb         (ByteBuffer/allocate byte-count)
                       buffer     (byte-array byte-count)]
                   (dotimes [n chunk-len]
                     ;; TODO: Find a more efficient way to do this
                     (doseq [arr val]
                       (.putShort bb (shortify (dbl/aget arr n)))))
                   (.position bb 0)
                   (.get bb buffer)
                   (>!! out buffer)
                   (recur))

                 :chunk-close nil

                 :error (or val :error-channel-closed))))
        (catch Throwable t t)))
     (async/close! out)
     (async/close! process))
    process))

(defn play
  "Plays `s` asynchronously. Returns a value that can be passed to `stop`."
  [s]
  (let [sample-rate 44100
        chans       (channels s)
        sdl         (AudioSystem/getSourceDataLine (AudioFormat. sample-rate
                                                                 16
                                                                 chans
                                                                 true
                                                                 true))
        chunks      (make-chan)
        provider    (sample-provider s sample-rate chunks)]
    {:player   (let [player (async/chan)]
                 (.open sdl)
                 (async/thread
                  (maybe-put
                   player
                   (try
                     (loop []
                       (let [[event val]
                             (async/alt!!
                              player ([m] [(if m :player-msg :player-close) m])
                              provider ([m] [(if m :provider-msg :provider-close) m])
                              chunks ([f] [(if f :chunk :chunk-close) f]))]
                         (case event
                           ;; TODO: Implement pause messages coming in
                           ;; For now, ignore messages arriving
                           :player-msg (recur)

                           :player-close nil

                           :provider-msg val

                           :provider-close nil

                           :chunk (let [buf ^bytes val]
                                    (.write sdl buf 0 (alength buf))
                                    (.start sdl) ;; Doesn't hurt to do it more than once
                                    (recur))

                           :chunk-close nil)))
                     (catch Throwable t t)))
                  (async/close! provider)
                  (async/close! player))
                 player)
     :provider provider
     :sdl      sdl}))

(defn stop
  "Stops playing the sound represented by `player` (returned from `play`)."
  [player]
  (async/close! (:player player)))

;;; Serialization

(defn- sampled-input-stream
  "Returns an implementation of `InputStream` over the data in `s`."
  [s sample-rate]
  (let [errors        (async/chan)
        ;; Empty chunks, while valid, will screw us over by causing us
        ;; to return zero from read
        useful-chunks (async/filter< (fn [[arr]] (-> arr dbl/alength (p/< 0)))
                                     (chunks s sample-rate errors)
                                     10)
        current-chunk (atom (take-chunk useful-chunks errors))
        offset        (atom 0)
        chans         (channels s)]
    (proxy [java.io.InputStream] []
      (available [] (-> (duration s) (* sample-rate) long (* (channels s) 2)))
      (close [])
      (mark [readLimit] (throw (UnsupportedOperationException.)))
      (markSupported [] false)
      (read ^int
        ([] (throw (ex-info "Not implemented" {:reason :not-implemented})))
        ([^bytes buf] (.read ^java.io.InputStream this buf 0 (alength buf)))
        ([^bytes buf off len]
           (let [head-chunk @current-chunk]
             (if-not head-chunk
               -1
               (let [chunk-frames               (dbl/alength (first head-chunk))
                     start-frame                (long @offset)
                     chunk-frames-remaining     (- chunk-frames start-frame)
                     chunk-bytes-remaining      (* chunk-frames-remaining 2 chans)
                     frames-requested           (/ len 2 chans)
                     read-remainder?            (<= chunk-frames-remaining frames-requested)
                     frames-to-read             (if read-remainder?
                                                  chunk-frames-remaining
                                                  frames-requested)
                     bytes-to-read              (if read-remainder? chunk-bytes-remaining len)
                     bb                         (ByteBuffer/allocate bytes-to-read)]
                 (when (zero? bytes-to-read)
                   (throw (ex-info "Zero bytes requested"
                                   {:reason                 :no-bytes-requested
                                    :off                    off
                                    :len                    len
                                    :start-frame            start-frame
                                    :chunk-frames           chunk-frames
                                    :chunk-frames-remaining chunk-frames-remaining
                                    :frames-requested       frames-requested
                                    :read-remainder?        read-remainder?
                                    :frames-to-read         frames-to-read
                                    :bytes-to-read          bytes-to-read})))
                 (dotimes [n frames-to-read]
                   ;; TODO: Find a more efficient way to do this
                   (doseq [arr head-chunk]
                     (.putShort bb (shortify (dbl/aget arr (p/+ start-frame n))))))
                 (.position bb 0)
                 (.get bb buf off bytes-to-read)
                 (if read-remainder?
                   (do (reset! current-chunk (take-chunk useful-chunks errors))
                       (reset! offset 0))
                   (swap! offset + frames-to-read))
                 bytes-to-read)))))
      (reset [] (throw (UnsupportedOperationException.)))
      (skip [n] (throw (ex-info "Not implemented" {:reason :not-implemented}))))))

(defn save
  "Save sound `s` to `path` as a 16-bit WAV at `sample-rate`."
  [s path sample-rate]
  (AudioSystem/write (AudioInputStream.
                      (sampled-input-stream s sample-rate)
                      (AudioFormat. sample-rate 16 (channels s) true true)
                      (-> s duration (* sample-rate) long))
                     AudioFileFormat$Type/WAVE
                     (io/file path)))


;;; Visualization

(defn- every-nth
  "Given a sequence of double arrays, return a collection holding
  every `n`th sample."
  [arrays period]
  (loop [remaining arrays
         n         period
         acc       []]
    (let [[head & more] remaining
          head-length (when head (dbl/alength head))]
      (if head
        (if (< n head-length)
          (recur remaining (+ n period) (conj acc (dbl/aget head n)))
          (recur more (- n head-length) acc))
        acc))))

;; TODO: There's definitely a protocol to be extracted here, assuming
;; the continuous-time stuff lives on.
(defn visualize
  "Visualizes channel `c` (default 0) of `s` by plotting it on a graph."
  ([s] (visualize s 0))
  ([s c]
     (let [num-data-points 4000
           ;; For short sounds, we need to sample at a higher rate, or
           ;; the graph won't be smooth enough. For longer sounds, we
           ;; can get away with a lower rate.
           sample-rate     (if (< (/ num-data-points 16000) (duration s))
                             16000
                             44100)
           errors          (chan)
           channel-chunks  (map #(nth % c) (chunk-seq (chunks s sample-rate errors) errors))
           num-samples     (-> s duration (* sample-rate) long)
           sample-period   (max 1 (-> num-samples (/ num-data-points) long))
           indexes         (range 0 num-samples sample-period)
           times           (map #(/ (double %) sample-rate) indexes)
           samples         (every-nth channel-chunks sample-period)]
       (incanter/view (charts/xy-plot
                       times
                       samples
                       :x-label "time (s)"
                       :y-label "amplitude"
                       :title (str "Amplitude for channel " c))))))
