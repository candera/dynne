(ns dynne.sound
  "Functions for working with sounds"
  (:require [clojure.java.io :as io]
            [incanter.core :as incanter]
            [incanter.charts :as charts]
            [primitive-math :as p])
  (:import [javax.sound.sampled
            AudioFileFormat$Type
            AudioFormat
            AudioFormat$Encoding
            AudioInputStream
            AudioSystem
            Clip]
           [dynne.sound.impl ISound MutableDouble BufferPosition]
           [java.util.concurrent LinkedBlockingQueue]))

;; We reserve the right to do something other than fall through to the
;; implementation, so we park some wrapper functions in this namespace.

(defmacro channels
  "Return the number of channels in `s`."
  ^long [s]
  `(.channels ^ISound ~s))

(defmacro duration
  "Return the duration of `s` in seconds."
  ^double [s]
  `(.duration ^ISound ~s))

(defmacro sample
  "Returns a vector of amplitues from sound `s` at time `t`, or zero
  if `t` falls outside the sound's range. Call this in preference to
  using the sound's `amplitude` implementation."
  [s t c]
  `(let [s# ^ISound ~s
         t# ^double ~t
         c# ^long ~c]
     (if (or (p/< t# 0.0) (p/< (.duration s#) t#))
       0.0
       (.amplitude s# t# c#))))

;; We're using a macro here because we can't have functions that take
;; primitives with more than four arguments.
(defmacro defoversampler
  "Defines a function called `name` that oversamples an input sound by
  a factor of `n`."
  [name n]
  `(defn ~name
     ~(str "Returns the mean of sampling `s` on channel `c` "
           n
           " steps of `delta-t` around `t`.")
    ^double [^ISound s# ^double t# ^long c# ^double delta-t#]
    (loop [acc# 0.0
           i# 0]
      (if (p/< i# ~n)
        (recur (p/+ acc# (sample s# (+ t# (p/* delta-t# (double i#))) c#))
               (p/+ i# 1))
        (/ acc# ~(double n))))))

(defoversampler oversample2 2)
(defoversampler oversample4 4)

;;; Sound construction

(defn ^ISound sound
  "Creates a sound `duration` seconds long where the amplitudes are
  produced by `f`. If `c`, the number of channels is not provided, it
  is assumed to be one, and `f` should accept only a single argument
  (the time). Otherwise, `f` must take a time and a channel number."
  ([^double duration f]
     (reify
       ISound
       (channels [this] 1)
       (duration [this] duration)
       (amplitude [this t c] (f t))))
  ([^double duration f ^long c]
     (reify
       ISound
       (channels [this] c)
       (duration [this] duration)
       (amplitude [this t c] (f t c)))))

(defn ^ISound null-sound
  "Returns a zero-duration sound with one channel."
  []
  (sound 0.0 (constantly 0.0)))

(defn ^ISound sinusoid
  "Returns a single-channel sound of `duration` and `frequency`."
  [^double duration ^double frequency]
  (sound duration
         (fn sinusoid-fn ^double [^double t]
           (Math/sin (p/* t frequency 2.0 Math/PI)))))

(defn ^ISound square-wave
  "Produces a single-channel sound that toggles between 1.0 and -1.0
  at frequency `freq`."
  [^double duration ^double freq]
  (sound duration
         (fn square-wav-fn ^double [^double t]
           (let [x (-> t (p/* freq 2.0) long)]
             (if (even? x) 1.0 -1.0)))))

(defn ^ISound linear
  "Produces a single-channel sound whose samples move linearly
  from `start` to `end` over `duration`."
  [^double duration ^double start ^double end]
  (let [span (double (- end start))]
    (sound duration
           (fn linear-fn ^double [^double t]
             (p/+ start (p/* span (/ t duration)))))))

(defn ^ISound silence
  "Creates a `n`-channel (default 1) sound that is `duration` long but silent."
  ([^double duration] (silence duration 1))
  ([^double duration ^long n] (sound duration (constantly 0.0) n)))

;;; File-based Sound

(defn- advance-frames
  "Reads and discards `n` frames from AudioInputStream `ais`. Returns
  the number of frames actually read."
  ^long [^AudioInputStream ais ^long n]
  (let [bytes-per-frame (-> ais .getFormat .getFrameSize)
        discard-frame-max 1000
        discard-buffer-bytes (p/* bytes-per-frame discard-frame-max)
        discard-buffer (byte-array discard-buffer-bytes)]
    (loop [total-frames-read (long 0)]
      (let [frames-left-to-read (- n total-frames-read)]
        (if (pos? frames-left-to-read)
          (let [frames-to-read (min discard-frame-max frames-left-to-read)
                bytes-to-read (* bytes-per-frame frames-to-read)
                bytes-read (.read ais discard-buffer (int 0) (int bytes-to-read))]
            (if (neg? bytes-read)
              total-frames-read
              (recur (p/+ total-frames-read (long (/ bytes-read bytes-per-frame))))))
          total-frames-read)))))

(defn ^ISound read-sound
  "Returns a Sound for the file at `path`."
  [path]
  (let [file                   (io/file path)
        in                     (atom (AudioSystem/getAudioInputStream file))
        base-format            (.getFormat ^AudioInputStream @in)
        base-file-format       (AudioSystem/getAudioFileFormat file)
        base-file-properties   (.properties base-file-format)
        base-file-duration     (get base-file-properties "duration")
        bits-per-sample        16
        bytes-per-sample       (long (/ bits-per-sample 8))
        channels               (.getChannels base-format)
        bytes-per-frame        (* bytes-per-sample channels)
        frames-per-second      (.getSampleRate base-format)
        decoded-format         (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                                             frames-per-second
                                             bits-per-sample
                                             channels
                                             (* bytes-per-sample channels)
                                             frames-per-second
                                             true)
        din                    (atom (AudioSystem/getAudioInputStream
                                      decoded-format
                                      ^AudioInputStream @in))
        decoded-length-seconds (if base-file-duration
                                 (/ base-file-duration 1000000.0)
                                 (/ (.getFrameLength ^AudioInputStream @din)
                                    frames-per-second))
        buffer-seconds         10
        buffer                 (byte-array (p/* frames-per-second
                                              buffer-seconds
                                              channels
                                              bytes-per-sample))
        buffer-pos             (BufferPosition. -1 -1)
        bb                     (java.nio.ByteBuffer/allocate bytes-per-frame)
        frame-array            (double-array channels)]
    (reify
      ISound
      (channels [s] channels)
      (duration [s] decoded-length-seconds)
      (amplitude [s t c]
        (if (or (p/< t 0.0) (p/< decoded-length-seconds t))
          0
          (let [frame-at-t ^long (-> t (p/* frames-per-second) long)]
            ;; Desired frame is before current buffer. Reset everything
            ;; to the start state
            (let [effective-start-of-buffer (.-start buffer-pos)]
              (when (p/< frame-at-t effective-start-of-buffer)
                ;;(println "rewinding")
                (.close ^AudioInputStream @din)
                (.close ^AudioInputStream @in)
                (reset! in (AudioSystem/getAudioInputStream (io/file path)))
                (reset! din (AudioSystem/getAudioInputStream decoded-format ^AudioInputStream @in))
                (set! (. buffer-pos start) -1)
                (set! (. buffer-pos end) -1)))

            ;; Desired position is past the end of the buffered region.
            ;; Update buffer to include it.
            (let [effective-end-of-buffer (.-end buffer-pos)]
              (when (p/< effective-end-of-buffer frame-at-t)
                (let [frames-to-advance (- frame-at-t effective-end-of-buffer 1)]
                  ;; We can't skip, because there's state built up during .read
                  ;; (println "Advancing to frame" frame-at-t
                  ;;          "by going forward" frames-to-advance
                  ;;          "frames")
                  (let [frames-advanced (advance-frames @din frames-to-advance)]
                    (let [bytes-read (.read ^AudioInputStream @din buffer)]
                      (if (pos? bytes-read)
                        (let [frames-read (/ bytes-read bytes-per-frame)
                              new-start-frame (p/+ (.-end buffer-pos) frames-advanced)]
                          (set! (. buffer-pos start) new-start-frame)
                          (set! (. buffer-pos end) (p/+ new-start-frame frames-read -1)))
                        (do
                          ;; We're at EOF, so just pretend we're right
                          ;; before where they asked for, so we don't
                          ;; get any false rewinds if they
                          ;; subsequently ask for the next frame
                          (set! (. buffer-pos start) (- frame-at-t 1))
                          (set! (. buffer-pos end) (- frame-at-t 1)))))))))

            ;; Now we're either positioned or the requested position
            ;; cannot be found
            (if (and (p/<= (.-start buffer-pos) frame-at-t)
                     (p/<= frame-at-t (.-end buffer-pos)))
              (let [buffer-frame-offset (p/- frame-at-t (.-start buffer-pos))
                    buffer-byte-offset (p/* buffer-frame-offset bytes-per-frame)]
                (.position bb 0)
                (.put bb buffer buffer-byte-offset bytes-per-frame)
                (.position bb (p/* c bytes-per-sample))
                ;; TODO: We're hardcoded to .getShort here, but the
                ;; bits-per-frame is a parameter. Should probably have
                ;; something that knows how to read from a ByteBuffer
                ;; given a number of bits.
                (/ (.getShort bb) 32768.0))
              0))))

      java.io.Closeable
      (close [this]
        (.close ^AudioInputStream @din)
        (.close ^AudioInputStream @in)))))


;;; Sound manipulation

(defn ^ISound multiplex
  "Uses `channel-map` (a map of source channel numbers to destination
  channel numbers) to return a sound where the channels have been so
  remapped."
  [^ISound s channel-map]
  (sound (duration s)
         (fn multiplex-fn ^double [^double t ^long c] (sample s t (get channel-map c)))
         (count (keys channel-map))))

(defn ^ISound ->stereo
  "Creates a stereo sound. If given one single-channel sound,
  duplicates channel zero on two channels. If given a single stereo
  sound, returns it. If given two single-channel sounds, returns a
  sound with the first sound on channel 0 and the second sound on
  channel 1."
  ([^ISound s]
           (case (long (channels s))
             1 (multiplex s {0 0, 1 0})
             2 s
             (throw (ex-info "Can't stereoize sounds with other than one or two channels"
                             {:reason :cant-stereoize-channels :s s}))))
  ([^ISound l ^ISound r]
     (when-not (= 1 (channels l) (channels r))
       (throw (ex-info "Can't steroize two sounds unless they are both single-channel"
                       {:reason :cant-stereoize-channels
                        :l-channels (channels l)
                        :r-channels (channels r)})))
     (sound (min (duration l) (duration r))
            (fn stereo-fn ^double [^double t ^long c]
              (case c
                0 (sample l t 0)
                1 (sample r t 1)))
            2)))

(defn ^ISound multiply
  [^ISound s1 ^ISound s2]
  "Multiplies two sounds together to produce a new one. Sounds must
  have the same number of channels."
  {:pre [(= (channels s1) (channels s2))]}
  (sound (min (duration s1) (duration s2))
         (fn multiply-fn ^double [^double t ^long c] (p/* (sample s1 t c) (sample s2 t c)))
         (channels s1)))

(defn ^ISound pan
  "Takes a two-channel sound and mixes the channels together by
  `amount`, a float on the range [0.0, 1.0]. The ususal use is to take
  a sound with separate left and right channels and combine them so
  each appears closer to stereo center. An `amount` of 0.0 would leave
  both channels unchanged, 0.5 would result in both channels being the
  same (i.e. appearing to be mixed to stereo center), and 1.0 would
  switch the channels."
  [^ISound s ^double amount]
  {:pre [(= 2 (channels s))]}
  (let [amount-complement (- 1.0 amount)]
    (sound (duration s)
           (fn pan-fn ^double [^double t ^long c]
             (let [s0 (sample s t 0)
                   s1 (sample s t 1)]
               (case c
                 0 (p/+ (p/* s0 amount-complement)
                        (p/* s1 amount))
                 1 (p/+ (p/* s0 amount)
                        (p/* s1 amount-complement)))))
           2)))

(defn ^ISound trim
  "Truncates `s` to the region between `start` and `end`."
  [^ISound s ^double start ^double end]
  (sound (min (duration s) (- end start))
         (fn trim-fn ^double [^double t ^long c] (sample s (p/+ t start) c))
         (channels s)))

(defn ^ISound mix
  "Mixes files `s1` and `s2`."
  [^ISound s1 ^ISound s2]
  {:pre [(= (channels s1) (channels s2))]}
  (sound (max (duration s1) (duration s2))
         (fn mix-fn ^double [^double t ^long c]
           (p/+ (sample s1 t c) (sample s2 t c)))
         (channels s1)))

(defn ^ISound append
  "Concatenates sounds together."
  [^ISound s1 ^ISound s2]
  {:pre [(= (channels s1) (channels s2))]}
  (let [d1 (duration s1)
        d2 (duration s2)]
    (sound (p/+ d1 d2)
           (fn append-fn ^double [^double t ^long c]
             (if (p/<= t d1)
               (sample s1 t c)
               (sample s2 (- t d1) c)))
           (channels s1))))

(defn ^ISound timeshift
  "Inserts `amount` seconds of silence at the beginning of `s`"
  [^ISound s ^double amount]
  (append (silence amount (channels s)) s))

(defn ^ISound channel-dup
  "Returns a sound that duplicates channel `n` (default 0) of `s2` on
  the same number of channels `s1` has"
  ([^ISound s1 ^ISound s2] (channel-dup s1 s2 0))
  ([^ISound s1 ^ISound s2 ^long n]
     (sound (duration s2)
            (fn channel-dup-fn ^double [^double t ^long c]
              (sample s2 t n))
            (channels s1))))

(defn ^ISound fade-in
  "Fades `s` linearly from zero at the beginning to full volume at
  `duration`."
  [^ISound s ^double fade-duration]
  (multiply s (channel-dup
               s
               (append (linear fade-duration 0 1.0)
                       (linear (- (duration s) fade-duration) 1.0 1.0)))))

(defn ^ISound fade-out
  "Fades the s to zero for the last `duration`."
  [^ISound s ^double fade-duration]
  (multiply s (channel-dup
               s
               (append (linear (- (duration s) fade-duration) 1.0 1.0)
                       (linear fade-duration 1.0 0.0)))))

(defn ^ISound segmented-linear
  "Produces a single-channels sound whose amplitudes change linear as
  described by `spec`. Spec is a sequence of interleaved amplitudes
  and durations. For example the spec

  1.0 30
  0   10
  0   0.5
  1.0

  (written that way on purpose - durations and amplitudes are in columns)
  would produce a sound whose amplitude starts at 1.0, linearly
  changes to 0.0 at time 30, stays at 0 for 10 seconds, then ramps up
  to its final value of 1.0 over 0.5 seconds"
  [& spec]
  {:pre [(and (odd? (count spec))
              (< 3 (count spec)))]}
  (->> spec
       (partition 3 2)
       (map (fn [[start duration end]] (linear duration start end)))
       (reduce append)))

(defn ^ISound gain
  "Returns a sound that is `s` scaled by `g`."
  [^ISound s ^double g]
  (sound (duration s)
         (fn gain-fn ^double [^double t ^long c]
           (p/* g (sample s t c)))
         (channels s)))

;;; Playback

(defmacro shortify
  "Takes a floating-point number f in the range [-1.0, 1.0] and scales
  it to the range of a 16-bit integer. Clamps any overflows."
  [f]
  (let [max-short-as-double (double Short/MAX_VALUE)]
    `(let [clamped# (-> ~f (min 1.0) (max -1.0))]
       (short (p/* ~max-short-as-double clamped#)))))

(defn- sample-provider
  "Returns a future that pushes byte arrays onto the
  LinkedBlockingQueue `q` for the sound `s`."
  [^ISound s ^LinkedBlockingQueue q ^long sample-rate]
  (let [channels     (channels s)
        buffer-bytes (p/* sample-rate channels) ;; Half-second
        bb           (java.nio.ByteBuffer/allocate buffer-bytes)
        total-frames (-> s duration (p/* (double sample-rate)) long)
        total-bytes  (p/* 2 total-frames channels)
        byte->t      (fn ^double [^long n] (-> n double (/ sample-rate channels 2)))]
   (future
     (loop [current-byte 0]
       (when (p/< current-byte total-bytes)
         (let [bytes-remaining (- total-bytes current-byte)
               bytes-to-write (min bytes-remaining buffer-bytes)
               buffer (byte-array bytes-to-write)]
           (.position bb 0)
           (doseq [i (range 0 bytes-to-write (p/* 2 channels))]
             (let [t  (byte->t (p/+ current-byte (long i)))]
               ;;(println t frame)
               (dotimes [c channels]
                 (let [samp (oversample4 s t c (/ 1.0 sample-rate 4.0))]
                   (.putShort bb (shortify samp))))))
           (.position bb 0)
           (.get bb buffer)
           ;; Bail if the player gets too far behind
           (when (.offer q buffer 2 java.util.concurrent.TimeUnit/SECONDS)
             (recur (+ current-byte bytes-to-write))))))
     (.put q ::eof))))

;; Oh shit: thread safety, because the underlying implementation over
;; files has state. OK, so don't do anything when a sound is playing.
(defn play
  "Plays `sound` asynchronously. Returns a value that can be passed to
  `stop`."
  [^ISound s]
  (let [sample-rate 44100
        channels    (channels s)
        sdl         (AudioSystem/getSourceDataLine (AudioFormat. sample-rate
                                                                 16
                                                                 channels
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

;;; Serialization

(defn- sampled-input-stream
  "Returns an implementation of `InputStream` over `s`."
  [^ISound s ^long sample-rate]
  (let [bits-per-sample   16
        ;;marked-position  (atom nil)
        current-t         (MutableDouble. 0.0)
        dur               (duration s)
        total-frames      (-> dur (* sample-rate) long)
        channels          (channels s)
        bytes-per-sample  (long (/ bits-per-sample 8))
        bytes-per-frame   (p/* channels bytes-per-sample)
        total-bytes       (p/* total-frames bytes-per-frame)
        seconds-per-frame (/ 1.0 sample-rate)]
    (proxy [java.io.InputStream] []
      (available [] (- total-bytes (-> dur (- (.-value current-t)) (p/* sample-rate bytes-per-frame))))
      (close [])
      (mark [readLimit] (throw (UnsupportedOperationException.)))
      (markSupported [] false)
      (read ^int
        ([] (throw (ex-info "Not implemented" {:reason :not-implemented})))
        ([^bytes buf] (read this buf 0 (alength buf)))
        ([^bytes buf off len]
           (if (p/< dur (.-value current-t))
             -1
             (let [start-t (.-value current-t)
                   seconds-remaining (- dur start-t)
                   buffer-seconds (/ (double len) bytes-per-frame sample-rate)
                   seconds-to-read (min buffer-seconds seconds-remaining)
                   bytes-to-read (-> seconds-to-read (p/* sample-rate bytes-per-frame) long)
                   bb (java.nio.ByteBuffer/allocate bytes-to-read)
                   frames-to-read (long (/ bytes-to-read bytes-per-frame))]
               (if-not (pos? frames-to-read)
                 -1
                 (dotimes [i frames-to-read]
                   (let [t (p/+ start-t (p/* (double i) seconds-per-frame))]
                     (dotimes [c channels]
                       (let [ ;; Oversample to smooth out some of the
                             ;; time-jitter that I think is introducing
                             ;; artifacts into the output a bit.
                             samp (oversample2 s t c (/ 1.0 sample-rate 4.0))]
                         (.putShort bb (shortify samp)))))))
               (.position bb 0)
               (.get bb buf off (p/* frames-to-read bytes-per-frame))
               (set! (. current-t value) (+ (.-value current-t) seconds-to-read))
               bytes-to-read))))
      (reset [] (throw (UnsupportedOperationException.)))
      (skip [n] (set! (.  current-t value) (+ (.-value current-t)
                                              (/ (double n) bytes-per-frame sample-rate)))))))

(defn save
  "Save sound `s` to `path` as a 16-bit WAV with `sample-rate`."
  [^ISound s ^String path ^long sample-rate]
  (AudioSystem/write (AudioInputStream.
                      (sampled-input-stream s sample-rate)
                      (AudioFormat. sample-rate 16 (channels s) true true)
                      (-> s duration (* sample-rate) long))
                     AudioFileFormat$Type/WAVE
                     (io/file path)))


;;; Visualization

(defn visualize
  "Visualizes `s` by plottig it on a graph."
  ([^ISound s] (visualize s 0))
  ([^ISound s ^long c]
     (let [duration (duration s)]
       ;; TODO: Maybe use a function that shows power in a window
       ;; around time t rather than just the sample
       (incanter/view (charts/function-plot #(sample s ^double % c)
                                            0.0
                                            duration
                                            :step-size (/ duration 4000.0))))))
