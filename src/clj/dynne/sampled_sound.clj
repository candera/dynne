(ns dynne.sampled-sound
  "Functions for manipulating a sound whose amplitude representation
  is arrays of doubles."
  (:require [clojure.java.io :as io]
            [hiphip.double :as dbl]
            [incanter.core :as incanter]
            [incanter.charts :as charts]
            [primitive-math :as p]))

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

(defn read-sound
  "Given a path to a .wav or .mp3 file, return a SampledSound instance
  over it."
  [path]
  (let [file                 (io/file path)
        base-file-format     (AudioSystem/getAudioFileFormat)
        base-file-properties (.properties base-file-properties)
        base-file-duration   (get base-file-properties "duration")
        is-wav?              (= AudioFileFormat$Type/WAVE (.getType base-file-format))
        chans                (.getChannels base-format)
        dur                  (/ base-file-duration 1000000.0)]
    (reify SampledSound
      (duration [this] dur)
      (channels [this] chans)
      (chunks [this sample-rate]
        (let [bits-per-sample 16
              bytes-per-sample (-> bits-per-sample (/ 8) long)
              compat?         (and is-wav? (= sample-rate
                                              (-> base-file-format .getFormat .getSampleRate)))
              din             (if compat?
                                (AudioSystem/getAudioInputStream file)
                                (AudioSystem/getAudioInputStream
                                 (AudioFormat. AudioFormat$Encoding/PCM_SIGNED
                                               sample-rate
                                               bits-per-sample
                                               chans
                                               (* bytes-per-sample chans)
                                               sample-rate
                                               true)
                                 ))])))))



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
  {:pre [(< 0 start end (duration s))]}
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

;; TODO: Generalize this, if it makes sense to do so. Which I'm
;; guessing it will, for enveloping, although that has a different
;; termination case.
(defn- add-chunks
  "Returns a sequence of chunks whose contents are the corresponding
  elements of `chunks1` and `chunks2` added together."
  [chunks1 ^long offset1 chunks2 ^long offset2]
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
         (map #(dbl/amake [i samples]
                          (p/+ (dbl/aget %1 (p/+ i offset1))
                               (dbl/aget %2 (p/+ i offset2))))
              head1
              head2)
         (add-chunks (if consumed1? more1 chunks1)
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
  "Mixes files `s1` and `s2` together."
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
          (add-chunks (chunks s1* sample-rate) 0
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

;; TODO: multiply

;; TODO: play

;; TODO: maybe make these into functions that return operations rather
;; than sounds.

;; (defn ->stereo
;;   "Creates a stereo sound. If given one single-channel sound,
;;   duplicates channel zero on two channels. If given a single stereo
;;   sound, returns it. If given two single-channel sounds, returns a
;;   sound with the first sound on channel 0 and the second sound on
;;   channel 1."
;;   ([s]
;;      (case (long (channels s))
;;        1 (reify SampledSound
;;            (channels [this] 2)
;;            (duration [this] (duration s))
;;            (amplitudes [this sample-rate]
;;              (let [amps (amplitudes s sample-rate)]
;;                [amps amps])))
;;        2 s
;;        (throw (ex-info "Can't stereoize sounds with other than one or two channels"
;;                        {:reason :cant-stereoize-channels :s s}))))
;;   ([l r]
;;      (when-not (= 1 (channels l) (channels r))
;;        (throw (ex-info "Can't steroize two sounds unless they are both single-channel"
;;                        {:reason :cant-stereoize-channels
;;                         :l-channels (channels l)
;;                         :r-channels (channels r)})))
;;      (reify SampledSound
;;        (channels [this] 2)
;;        (duration [this] (min (duration l) (duration r)))
;;        (amplitudes [this sample-rate]
;;          (let [l-samples (nth (amplitudes l sample-rate) 0)
;;                r-samples (nth (amplitudes r sample-rate) 0)]
;;            ;; TODO: Truncate the longer one
;;            [l r])))))

;; TODO: pan

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
     (let [sample-rate 16000
           num-data-points 4000
           channel-chunks (map #(nth % c) (chunks s sample-rate))
           num-samples (-> s duration (* sample-rate) long)
           sample-period (-> num-samples (/ num-data-points) long)
           indexes (range 0 num-samples sample-period)
           times (map #(/ (double %) sample-rate) indexes)
           samples (every-nth channel-chunks sample-period)]
       (incanter/view (charts/xy-plot
                       times
                       samples
                       :x-label "time (s)"
                       :y-label "amplitude"
                       :title (str "Amplitude for channel " c))))))
