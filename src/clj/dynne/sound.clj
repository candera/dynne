(ns dynne.sound
  "Functions for working with sounds"
  (:require [clojure.java.io :as io]
            [incanter.core :as incanter]
            [incanter.charts :as charts])
  (:import [javax.sound.sampled
            AudioFileFormat$Type
            AudioFormat
            AudioFormat$Encoding
            AudioInputStream
            AudioSystem
            Clip]
           [dynne.sound.impl ISound]))

;; We reserve the right to do something other than fall through to the
;; implementation, so we park some wrapper functions in this namespace.

(defn channels
  "Return the number of channels in `s`."
  ^long [^ISound s]
  (.channels s))

(defn duration
  "Return the duration of `s` in seconds."
  ^double [^ISound s]
  (.duration s))

(defn sample
  "Returns a vector of amplitues from sound `s` at time `t`, or zero
  if `t` falls outside the sound's range. Call this in preference to
  using the sound's `amplitude` implementation."
  ^double [^ISound s t c]
  (if (or (< t 0.0) (< (.duration s) t))
    0.0
    (.amplitude s t c)))

;; n is fixed at four because we can't have functions that take
;; primitives with more than four arguments. TODO: Maybe do something
;; about this.
(defn oversample4
  "Returns the mean of sampling `s` on channel `c` 4 steps of `delta-t` around `t`."
  ^double [^ISound s t c delta-t]
  (loop [acc 0.0
         i 0]
    (if (< i 4)
      (recur (+ acc (sample s (+ t (* delta-t (double i))) c))
             (+ i 1))
      (/ acc 4.0))))

;;; Sound construction

(defn sound
  "Creates a sound `duration` seconds long where the amplitudes are
  produced by `f`. If `c`, the number of channels is not provided, it
  is assumed to be one, and `f` should accept only a single argument
  (the time). Otherwise, `f` must take a time and a channel number."
  ([duration f]
     (reify
       ISound
       (channels [this] 1)
       (duration [this] duration)
       (amplitude [this t c] (f t))))
  ([duration f c]
     (reify
       ISound
       (channels [this] c)
       (duration [this] duration)
       (amplitude [this t c] (f t c)))))

(defn null-sound
  "Returns a zero-duration sound with one channel."
  []
  (sound 0.0 (constantly 0.0)))

(defn sinusoid
  "Returns a single-channel sound of `duration` and `frequency`."
  [duration ^double frequency]
  (sound duration
         (fn [^double t]
           (Math/sin (* t frequency 2.0 Math/PI)))))

(defn square-wave
  "Produces a single-channel sound that toggles between 1.0 and -1.0
  at frequency `freq`."
  [duration freq]
  (sound duration
         (fn [t]
           (let [x (-> t (* freq 2.0) long)]
             (if (even? x) 1.0 -1.0)))))

(defn linear
  "Produces a single-channel sound whose samples move linearly
  from `start` to `end` over `duration`."
  [^double duration ^double start ^double end]
  (let [span (double (- end start))]
    (sound duration
           (fn [^double t]
             (+ start (* span (/ t duration)))))))

(defn silence
  "Creates a single-channel sound that is `duration` long but silent."
  [duration] (sound duration (constantly 0.0)))

;;; File-based Sound

(defn- advance-frames
  "Reads and discards `n` frames from AudioInputStream `ais`. Returns
  the number of frames actually read."
  [^AudioInputStream ais n]
  (let [bytes-per-frame (-> ais .getFormat .getFrameSize)
        discard-frame-max 1000
        discard-buffer-bytes (* bytes-per-frame discard-frame-max)
        discard-buffer (byte-array discard-buffer-bytes)]
    (loop [total-frames-read (long 0)]
      (let [frames-left-to-read (- n total-frames-read)]
        (if (pos? frames-left-to-read)
          (let [frames-to-read (min discard-frame-max frames-left-to-read)
                bytes-to-read (* bytes-per-frame frames-to-read)
                bytes-read (.read ais discard-buffer (int 0) (int bytes-to-read))]
            (if (neg? bytes-read)
              total-frames-read
              (recur (+ total-frames-read (long (/ bytes-read bytes-per-frame))))))
          total-frames-read)))))

(defn read-sound
  "Returns a Sound for the file at `path`."
  [path]
  (let [file                   (io/file path)
        in                     (atom (AudioSystem/getAudioInputStream file))
        base-format            (.getFormat ^AudioInputStream @in)
        base-file-format       (AudioSystem/getAudioFileFormat file)
        base-file-properties   (.properties base-file-format)
        base-file-duration     (get base-file-properties "duration")
        bits-per-sample        16
        bytes-per-sample       (/ bits-per-sample 8)
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
        buffer                 (byte-array (* frames-per-second
                                              buffer-seconds
                                              channels
                                              bytes-per-sample))
        buffer-pos             (dynne.sound.impl.BufferPosition. -1 -1)
        bb                     (java.nio.ByteBuffer/allocate bytes-per-frame)
        frame-array            (double-array channels)]
    (reify
      ISound
      (channels [s] channels)
      (duration [s] decoded-length-seconds)
      (amplitude [s t c]
        (if (or (< t 0.0) (< decoded-length-seconds t))
          0
          (let [frame-at-t (-> t (* frames-per-second) long)]
            ;; Desired frame is before current buffer. Reset everything
            ;; to the start state
            (let [effective-start-of-buffer (.-start buffer-pos)]
              (when (< frame-at-t effective-start-of-buffer)
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
              (when (< effective-end-of-buffer frame-at-t)
                (let [frames-to-advance (- frame-at-t effective-end-of-buffer 1)]
                  ;; We can't skip, because there's state built up during .read
                  ;; (println "Advancing to frame" frame-at-t
                  ;;          "by going forward" frames-to-advance
                  ;;          "frames")
                  (let [frames-advanced (advance-frames @din frames-to-advance)]
                    (let [bytes-read (.read ^AudioInputStream @din buffer)]
                      (if (pos? bytes-read)
                        (let [frames-read (/ bytes-read bytes-per-frame)
                              new-start-frame (+ (.-end buffer-pos) frames-advanced)]
                          (set! (. buffer-pos start) new-start-frame)
                          (set! (. buffer-pos end) (+ new-start-frame frames-read -1)))
                        (do
                          ;; We're at EOF, so just pretend we're right
                          ;; before where they asked for, so we don't
                          ;; get any false rewinds if they
                          ;; subsequently ask for the next frame
                          (set! (. buffer-pos start) (- frame-at-t 1))
                          (set! (. buffer-pos end) (- frame-at-t 1)))))))))

            ;; Now we're either positioned or the requested position
            ;; cannot be found
            (if (and (<= (.-start buffer-pos) frame-at-t)
                     (<= frame-at-t (.-end buffer-pos)))
              (let [buffer-frame-offset (- frame-at-t (.-start buffer-pos))
                    buffer-byte-offset (* buffer-frame-offset bytes-per-frame)]
                (.position bb 0)
                (.put bb buffer buffer-byte-offset bytes-per-frame)
                (.position bb (* c bytes-per-sample))
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

(defn multiplex
  "Uses `channel-map` (a map of source channel numbers to destination
  channel numbers) to return a sound where the channels have been so
  remapped."
  [s channel-map]
  (sound (duration s)
         (fn [t c] (sample s t (get channel-map c)))
         (count (keys channel-map))))

(defn ->stereo
  "Creates a stereo sound. If given one single-channel sound,
  duplicates channel zero on two channels. If given a single stereo
  sound, returns it. If given two single-channel sounds, returns a
  sound with the first sound on channel 0 and the second sound on
  channel 1."
  ([s] (case (long (channels s))
         1 (multiplex s {0 0, 1 0})
         2 s
         (throw (ex-info "Can't stereoize sounds with other than one or two channels"
                         {:reason :cant-stereoize-channels :s s}))))
  ([l r]
     (when-not (= 1 (channels l) (channels r))
       (throw (ex-info "Can't steroize two sounds unless they are both single-channel"
                       {:reason :cant-stereoize-channels
                        :l-channels (channels l)
                        :r-channels (channels r)})))
     (sound (min (duration l) (duration r))
            (fn [^double t ^long c]
              (case c
                0 (sample l t 0)
                1 (sample r t 1)))
            2)))

(defn multiply
  [s1 s2]
  "Multiplies two sounds together to produce a new one. Sounds must
  have the same number of channels."
  {:pre [(= (channels s1) (channels s2))]}
  (sound (min (duration s1) (duration s2))
         (fn [^double t ^long c] (* (sample s1 t c) (sample s2 t c)))
         (channels s1)))

(defn pan
  "Takes a two-channel sound and mixes the channels together by
  `amount`, a float on the range [0.0, 1.0]. The ususal use is to take
  a sound with separate left and right channels and combine them so
  each appears closer to stereo center. An `amount` of 0.0 would leave
  both channels unchanged, 0.5 would result in both channels being the
  same (i.e. appearing to be mixed to stereo center), and 1.0 would
  switch the channels."
  [s amount]
  {:pre [(= 2 (channels s))]}
  (let [amount-complement (- 1.0 amount)]
    (sound (duration s)
           (fn [^double t ^long c]
             (let [s0 (sample s t 0)
                   s1 (sample s t 1)]
               (case c
                 0 (+ (* s0 amount-complement)
                      (* s1 amount))
                 1 (+ (* s0 amount)
                      (* s1 amount-complement)))))
           2)))

(defn trim
  "Truncates `s` to the region between `start` and `end`."
  [s start end]
  (sound (min (duration s) (- end start))
         (fn [^double t c] (sample s (+ t start) c))
         (channels s)))

(defn mix
  "Mixes files `s1` and `s2`."
  [s1 s2]
  {:pre [(= (channels s1) (channels s2))]}
  (sound (max (duration s1) (duration s2))
         (fn [t c]
           (+ (sample s1 t c) (sample s2 t c)))
         (channels s1)))

(defn append
  "Concatenates sounds together."
  [s1 s2]
  {:pre [(= (channels s1) (channels s2))]}
  (let [d1 (duration s1)
        d2 (duration s2)]
    (sound (+ d1 d2)
           (fn [^double t ^long c]
             (if (<= t d1)
               (sample s1 t c)
               (sample s2 (- t d1) c)))
           (channels s1))))

(defn timeshift
  "Inserts `amount` seconds of silence at the beginning of `s`"
  [s amount]
  (append (silence amount (channels s)) s))

(defn channel-dup
  "Returns a sound that duplicates channel `n` (default 0) of `s2` on
  the same number of channels `s1` has"
  ([s1 s2] (channel-dup s1 s2 0))
  ([s1 s2 n]
     (sound (duration s2)
            (fn [^double t ^long c]
              (sample s2 t n))
            (channels s1))))

(defn fade-in
  "Fades `s` linearly from zero at the beginning to full volume at
  `duration`."
  [^ISound s ^double fade-duration]
  (multiply s (channel-dup
               s
               (append (linear fade-duration 0 1.0)
                       (linear (- (duration s) fade-duration) 1.0 1.0)))))

(defn fade-out
  "Fades the s to zero for the last `duration`."
  [s fade-duration]
  (multiply s (channel-dup
               s
               (append (linear (- (duration s) fade-duration) 1.0 1.0)
                       (linear fade-duration 1.0 0.0)))))

(defn segmented-linear
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

;;; Playback

(defn short-sample
  "Takes a floating-point number f in the range [-1.0, 1.0] and scales
  it to the range of a 16-bit integer. Clamps any overflows."
  [f]
  (let [f* (-> f (min 1.0) (max -1.0))]
    (short (* Short/MAX_VALUE f*))))

(defn- sample-provider
  "Returns a future that pushes byte arrays onto the
  LinkedBlockingQueue `q` for the sound `s`."
  [s q sample-rate]
  (let [channels     (channels s)
        buffer-bytes (* sample-rate channels) ;; Half-second
        bb           (java.nio.ByteBuffer/allocate buffer-bytes)
        total-bytes  (-> s duration (* sample-rate) long (* channels 2))
        byte->t      (fn [n] (-> n double (/ sample-rate channels 2)))]
   (future
     (loop [current-byte 0]
       (when (< current-byte total-bytes)
         (let [bytes-remaining (- total-bytes current-byte)
               bytes-to-write (min bytes-remaining buffer-bytes)
               buffer (byte-array bytes-to-write)]
           (.position bb 0)
           (doseq [i (range 0 bytes-to-write (* 2 channels))]
             (let [t  (byte->t (+ current-byte i))]
               ;;(println t frame)
               (dotimes [c channels]
                 (let [samp (oversample4 s t c (/ 1.0 sample-rate 4.0))]
                   (.putShort bb (short-sample samp))))))
           (.position bb 0)
           (.get bb buffer)
           ;; Bail if the player gets too far behind
           (when (.offer q buffer 2 java.util.concurrent.TimeUnit/SECONDS)
             (recur (+ current-byte bytes-to-write))))))
     (.put q ::eof))))

;; Oh shit: thread safety, because the underlying implementation over
;; files has state. OK, so don't do anything when a sound is playing.
(defn play
  "Plays `sound`."
  [s]
  (let [sample-rate 44100
        channels    (channels s)
        sdl         (AudioSystem/getSourceDataLine (AudioFormat. sample-rate
                                                                 16
                                                                 channels
                                                                 true
                                                                 true))
        stopped     (atom false)
        q           (java.util.concurrent.LinkedBlockingQueue. 10)
        provider    (sample-provider s q sample-rate)]
    {:player   (future (.open sdl)
                       (loop [buf (.take q)]
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


;;; Serialization

(defn- sampled-input-stream
  "Returns an implementation of `InputStream` over `s`."
  [s sample-rate]
  (let [bits-per-sample   16
        marked-position  (atom nil)
        bytes-read       (atom 0)
        total-frames     (-> s duration (* sample-rate) long)
        channels         (channels s)
        bytes-per-sample (/ bits-per-sample 8)
        bytes-per-frame  (* channels bytes-per-sample)
        total-bytes      (* total-frames bytes-per-frame)]
    (proxy [java.io.InputStream] []
      (available [] (- total-bytes @bytes-read))
      (close [])
      (mark [readLimit] (reset! marked-position @bytes-read))
      (markSupported [] true)
      (read ^int
        ([] (throw (ex-info "Not implemented" {:reason :not-implemented})))
        ([^bytes buf] (read this buf 0 (alength buf)))
        ([^bytes buf off len]
           (if (<= total-bytes @bytes-read)
             -1
             (let [frames-to-read (/ len bytes-per-frame)
                   bytes-remaining (- total-bytes @bytes-read)
                   bytes-to-read (min len bytes-remaining)
                   bb (java.nio.ByteBuffer/allocate bytes-to-read)]
               (dotimes [i (long (/ len bytes-per-frame))]
                 (let [t (/ (double (+ i @bytes-read)) (* bytes-per-frame sample-rate))]
                   (dotimes [c channels]
                     (let [;; Oversample to smooth out some of the
                           ;; time-jitter that I think is introducing
                           ;; artifacts into the output a bit.
                           samp (oversample4 s t c (/ 1.0 sample-rate 4.0))]
                       (.putShort bb (* samp Short/MAX_VALUE))))))
               (.position bb 0)
               (.get bb buf off len)
               (swap! bytes-read + bytes-to-read)
               bytes-to-read))))
      (reset [] (reset! bytes-read @marked-position))
      (skip [n] (swap! bytes-read + n)))))

(defn save
  "Save sound `s` to `path` as a 16-bit WAV with `sample-rate`."
  [s path sample-rate]
  (AudioSystem/write (AudioInputStream.
                      (sampled-input-stream s sample-rate)
                      (AudioFormat. sample-rate 16 (channels s) true true)
                      (-> s duration (* sample-rate) long))
                     AudioFileFormat$Type/WAVE
                     (io/file path)))


;;; Visualization

(defn visualize
  "Visualizes `s` by plottig it on a graph."
  ([s] (visualize s 0))
  ([s c]
     (let [duration (duration s)]
       ;; TODO: Maybe use a function that shows power in a window
       ;; around time t rather than just the sample
       (incanter/view (charts/function-plot #(sample s % c)
                                            0.0
                                            duration
                                            :step-size (/ duration 4000.0))))))
