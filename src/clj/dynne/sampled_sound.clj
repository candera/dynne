(ns dynne.sampled-sound
  "Functions for manipulating a sound whose amplitude representation
  is arrays of doubles."
  (:require [clojure.java.io :as io]
            [hiphip.double :as dbl]
            [incanter.core :as incanter]
            [incanter.charts :as charts]
            [primitive-math :as p])
  (:import [java.util.concurrent LinkedBlockingQueue]
           [javax.sound.sampled
            AudioFileFormat$Type
            AudioFormat
            AudioFormat$Encoding
            AudioInputStream
            AudioSystem]))

;;; Abstraction

;; TODO: It feels like the channels and duration stuff are the real
;; core of the abstraction, and the way you get amplitudes is sort of
;; orthogonal. Maybe there's another abstraction that can get pulled
;; out here.

(defprotocol SampledSound
  "Represents a sound as a sequence of vectors of Java double arrays."
  (channels [this] "Returns the number of channels in the sound.")
  (duration [this] "Returns the duration of the sound in seconds.")
  (chunks [this sample-rate] "Returns a sequence of sequences each
  containing a sequence of double arrays - one per channel - populated
  with the data for this sound. The total number of samples per
  channel will be (* duration sample-rate)"))

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
       (reify SampledSound
         (channels [this#] ~channels-param)
         (duration [this#] duration#)
         (chunks [this# ~sample-rate]
           (let [chunk-size#  10000
                 ~num-samples (long (* duration# ~sample-rate))
                 num-chunks#  (-> ~num-samples (/ chunk-size#) Math/ceil long)]
             (concat
              (for [chunk-num# (range (dec num-chunks#))]
                (let [base-index# (p/* (long chunk-num#) chunk-size#)]
                  (for [~c (range chans#)]
                    (dbl/amake [i# chunk-size#]
                               (let [~index (p/+ i# base-index#)]
                                 ~expr)))))
              ;; Handle the last chunk specially, since it's probably shorter
              [(for [~c (range chans#)]
                 (dbl/amake [i# (mod ~num-samples chunk-size#)]
                            (let [~index (p/+ i# (p/* (p/- num-chunks# 1) chunk-size#))]
                                 ~expr)))])))))))

(defsound constant duration chans
  "Returns a sound of `duration` that has `chans` channels, each of
  which is constant at `x`."
  [x]
  [sample-rate num-samples i c]
  x)

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


(defn- to-double-arrays
  "Return a seq of arrays of doubles that decode the values in buf."
  [^bytes buf ^long bytes-read ^long bytes-per-sample ^long chans]
  (let [samples-read (/ bytes-read bytes-per-sample chans)
        bb           (java.nio.ByteBuffer/allocate bytes-read)
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

(defn- sample-chunks
  "Return a seq of chunks from an AudioInputStream."
  [^AudioInputStream ais ^long chans ^long bytes-per-sample ^long chunk-size]
  (let [buf (byte-array (p/* chunk-size chans bytes-per-sample))
        bytes-read (.read ais buf)]
    (when (pos? bytes-read)
      (lazy-seq
       (cons (to-double-arrays buf (long bytes-read) bytes-per-sample chans)
             (sample-chunks ais chans bytes-per-sample chunk-size))))))

(defn read-sound
  "Given a path to a .wav or .mp3 file, return a SampledSound instance
  over it."
  [path]
  (let [file                 (io/file path)
        base-file-format     (AudioSystem/getAudioFileFormat file)
        base-file-properties (.properties base-file-format)
        base-file-duration   (get base-file-properties "duration")
        is-wav?              (= AudioFileFormat$Type/WAVE (.getType base-file-format))
        chans                (-> base-file-format .getFormat .getChannels)
        dur                  (/ base-file-duration 1000000.0)]
    (reify SampledSound
      (duration [this] dur)
      (channels [this] chans)
      (chunks [this sample-rate]
        (let [bits-per-sample  16
              bytes-per-sample (-> bits-per-sample (/ 8) long)
              in               (AudioSystem/getAudioInputStream file)
              decoded-format   (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                                             sample-rate
                                             bits-per-sample
                                             chans
                                             (* bytes-per-sample chans)
                                             sample-rate
                                             true)
              din              (AudioSystem/getAudioInputStream
                                decoded-format
                                ^AudioInputStream in)]
          (sample-chunks din chans bytes-per-sample 10000))))))

;;; Sound manipulation

(defn append
  "Concatenates two sounds together"
  [s1 s2]
  {:pre [(= (channels s1) (channels s2))]}
  (let [d1 (duration s1)
        d2 (duration s2)]
    (reify SampledSound
      (duration [this] (+ d1 d2))
      (channels [this] (channels s1))
      (chunks [this sample-rate]
        (concat (chunks s1 sample-rate)
                (chunks s2 sample-rate))))))

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
   (zero? n) nil

   (< n (dbl/alength (ffirst chunks)))
   [(map #(dbl-asub % 0 n) (first chunks))]

   (seq chunks)
   (cons (first chunks)
         (take-samples (- n (dbl/alength (ffirst chunks)))
                       (rest chunks)))))

(defn trim
  "Truncates `s` to the region between `start` and `end`."
  [s ^double start ^double end]
  {:pre [(<= 0 start end (duration s))]}
  (let [dur (min (duration s) (- end start))]
    (reify SampledSound
      (duration [this] dur)
      (channels [this] (channels s))
      (chunks [this sample-rate]
        (let [samples-to-drop (-> start (* sample-rate) long)
              samples-to-take (-> end (p/- start) (* sample-rate))]
          (->> (chunks s sample-rate)
               (drop-samples samples-to-drop)
               (take-samples samples-to-take)))))))

(defn- combine-chunks
  "Returns a sequence of chunks whose contents are corresponding
  elements of chunks1 and chunks2 combined by calling `f` on them. `f`
  should be a function of the number of samples in the chunk to be
  produced, the first chunk, the offset in that chunk at which to
  start, the second chunk, and the offset in that chunk at which to
  start."
  [f chunks1 offset1 chunks2 offset2]
  (let [[head1 & more1] chunks1
        [head2 & more2] chunks2]
    (cond
     (and head1 head2)
     (let [len1       (dbl/alength (first head1))
           len2       (dbl/alength (first head2))
           samples    (min (- len1 offset1) (- len2 offset2))
           consumed1? (= len1 (+ samples offset1))
           consumed2? (= len2 (+ samples offset2))]
       (lazy-seq
        (cons
         (f samples head1 offset1 head2 offset2)
         (combine-chunks f
                         (if consumed1? more1 chunks1)
                         (if consumed1? 0 (+ offset1 samples))
                         (if consumed2? more2 chunks2)
                         (if consumed2? 0 (+ offset2 samples))))))

     (and head1 (not head2))
     (cons (map #(dbl-asub offset1 (dbl/alength head1)) head1)
           more1)

     (and (not head1) head2)
     (cons (map #(dbl-asub offset2 (dbl/alength head2)) head2)
           more2))))

(defn mix
  "Mixes sounds `s1` and `s2` together."
  [s1 s2]
  {:pre [(= (channels s1) (channels s2))]}
  (let [d1 (duration s1)
        d2 (duration s2)]
    (reify SampledSound
      (duration [this] (max d1 d2))
      (channels [this] (channels s1))
      (chunks [this sample-rate]
        (let [s1* (if (< d1 d2)
                    (append s1 (constant (- d2 d1) (channels s1) 0.0))
                    s1)
              s2* (if (<= d1 d2)
                    s2
                    (append (chunks s2 sample-rate)
                            (constant (- d1 d2) (channels s2) 0.0)))]
          (combine-chunks (fn [samples head1 offset1 head2 offset2]
                            (map #(dbl/amake [i samples]
                                             (p/+ (dbl/aget %1 (p/+ i (long offset1)))
                                                  (dbl/aget %2 (p/+ i (long offset2)))))
                                 head1
                                 head2))
                          (chunks s1* sample-rate)
                          0
                          (chunks s2* sample-rate) 0))))))

(defn gain
  "Changes the amplitude of `s` by `g`."
  [s ^double g]
  (reify SampledSound
    (duration [this] (duration s))
    (channels [this] (channels s))
    (chunks [this sample-rate]
      (map (fn [chunk]
             (map (fn [channel-chunk]
                    (dbl/amap [x channel-chunk]
                              (p/* x g)))
                  chunk))
           (chunks s sample-rate)))))

;; TODO: envelope

(defn ->stereo
  "Creates a stereo sound. If given one single-channel sound,
  duplicates channel zero on two channels. If given a single stereo
  sound, returns it. If given two single-channel sounds, returns a
  sound with the first sound on channel 0 and the second sound on
  channel 1."
  ([s]
     (case (long (channels s))
       2 s
       1 (reify SampledSound
           (duration [this] (duration s))
           (channels [this] 2)
           (chunks [this sample-rate]
             (map vector (chunks s sample-rate) (chunks s sample-rate))))
       (throw (ex-info "Can't steroize sound with other than one or two channels"
                       {:reason :cant-stereoize-channels
                        :s      s}))))
  ([l r]
     (when-not (= 1 (channels l) (channels r))
       (throw (ex-info "Can't steroize two sounds unless they are both single-channel"
                       {:reason :cant-stereoize-channels
                        :l-channels (channels l)
                        :r-channels (channels r)})))
     (reify SampledSound
       (duration [this] (min (duration l) (duration r)))
       (channels [this] 2)
       (chunks [this sample-rate]
         (combine-chunks (fn [samples [head1] offset1 [head2] offset2]
                           [(dbl-asub head1 offset1 (+ offset1 samples))
                            (dbl-asub head2 offset2 (+ offset2 samples))])
                         (chunks l sample-rate)
                         0
                         (chunks r sample-rate)
                         0)))))

;; TODO: pan

;; TODO: maybe make these into functions that return operations rather
;; than sounds.

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

(defn- sample-provider
  [s ^LinkedBlockingQueue q ^long sample-rate]
  (let [chans          (channels s)]
    (future
      (loop [[head-chunk & more] (chunks s sample-rate)]
        (if-not head-chunk
          (.put q ::eof)
          (let [chunk-len  (dbl/alength (first head-chunk))
                byte-count (p/* chans 2 chunk-len)
                bb         (java.nio.ByteBuffer/allocate byte-count)
                buffer     (byte-array byte-count)]
            (dotimes [n chunk-len]
              ;; TODO: Find a more efficient way to do this
              (doseq [arr head-chunk]
                (.putShort bb (shortify (dbl/aget arr n)))))
            (.position bb 0)
            (.get bb buffer)
            ;; Bail if the player gets too far behind
            (when (.offer q buffer 2 java.util.concurrent.TimeUnit/SECONDS)
              (recur more))))))))

;; TODO: This is identical to the one in sound.clj. Merge them if we
;; don't get rid of sound.clj
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
        stopped     (atom false)
        q           (LinkedBlockingQueue. 10)
        provider    (sample-provider s q sample-rate)]
    {:player   (future (.open sdl)
                       (loop [buf ^bytes (.take q)]
                         (when-not (or @stopped (= buf ::eof))
                           (.write sdl buf 0 (alength buf))
                           (.start sdl) ;; Doesn't hurt to do it more than once
                           (recur (.take q)))))
     :stop     (fn []
                 (reset! stopped true)
                 (future-cancel provider)
                 (.stop sdl))
     :q        q
     :provider provider
     :sdl      sdl}))

(defn stop
  "Stops playing the sound represented by `player` (returned from `play`)."
  [player]
  ((:stop player)))

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
     (let [sample-rate 44100
           num-data-points 4000
           channel-chunks (map #(nth % c) (chunks s sample-rate))
           num-samples (-> s duration (* sample-rate) long)
           sample-period (max 1 (-> num-samples (/ num-data-points) long))
           indexes (range 0 num-samples sample-period)
           times (map #(/ (double %) sample-rate) indexes)
           samples (every-nth channel-chunks sample-period)]
       (incanter/view (charts/xy-plot
                       times
                       samples
                       :x-label "time (s)"
                       :y-label "amplitude"
                       :title (str "Amplitude for channel " c))))))
