(ns dynne.sampled-sound
  "Functions for manipulating a sound whose amplitude representation
  is arrays of doubles."
  (:require [hiphip.double :as dbl]
            [incanter.core :as incanter]
            [incanter.charts :as charts]
            [primitive-math :as p]))

;;; Abstraction

;; TODO: It feels like the channels and duration stuff are the real
;; core of the abstraction, and the way you get amplitudes is sort of
;; orthogonal.

(defprotocol SampledSound
  "Represents a sound as a sequence of vectors of Java double arrays."
  (channels [this] "Returns the number of channels in the sound.")
  (duration [this] "Returns the duration of the sound in seconds.")
  (amplitudes [this sample-rate] "Returns a function, which, when called, will return a vector of Java
  double arrays, one per channel, populated with the data for this
  sound."))

;;; Sound construction

(defn ^SampledSound sound
  "Creates a SampledSound `duration` seconds long where the amplitudes
  are produced by `f`, a function of a channel number and a time in
  seconds."
  [^double duration ^long chans f]
  (reify SampledSound
    (channels [this] chans)
    (duration [this] duration)
    (amplitudes [this sample-rate]
      (for [c (range chans)]
        (dbl/amake [i (* duration sample-rate)]
                   (f c (-> i (/ sample-rate) double)))))))

(defn sinusoid
  "Returns a single-channel sound of `duration` and `frequency`"
  [^double duration ^double frequency]
  (sound duration 1 (fn sinusoid-fn [^long c ^double t]
                      (Math/sin (p/* t frequency 2.0 Math/PI)))))

;;; Sound manipulation

;; TODO: maybe make these into functions that return operations rather
;; than sounds.

(defn ->stereo
  "Creates a stereo sound. If given one single-channel sound,
  duplicates channel zero on two channels. If given a single stereo
  sound, returns it. If given two single-channel sounds, returns a
  sound with the first sound on channel 0 and the second sound on
  channel 1."
  ([s]
     (case (long (channels s))
       1 (reify SampledSound
           (channels [this] 2)
           (duration [this] (duration s))
           (amplitudes [this sample-rate]
             (let [amps (amplitudes s sample-rate)]
               [amps amps])))
       2 s
       (throw (ex-info "Can't stereoize sounds with other than one or two channels"
                       {:reason :cant-stereoize-channels :s s}))))
  ([l r]
     (when-not (= 1 (channels l) (channels r))
       (throw (ex-info "Can't steroize two sounds unless they are both single-channel"
                       {:reason :cant-stereoize-channels
                        :l-channels (channels l)
                        :r-channels (channels r)})))
     (reify SampledSound
       (channels [this] 2)
       (duration [this] (min (duration l) (duration r)))
       (amplitudes [this sample-rate]
         (let [l-samples (nth (amplitudes l sample-rate) 0)
               r-samples (nth (amplitudes r sample-rate) 0)]
           ;; TODO: Truncate the longer one
           [l r])))))

;; TODO: pan

(defn trim
  "Truncates `s` to the region between `start` and `end`."
  [s ^double start ^double end]
  (let [dur (min (duration s) (- end start))]
    (reify SampledSound
      (channels [this] (channels s))
      (duration [this] dur)
      (amplitudes [this sample-rate]
        (let [amps (amplitudes s sample-rate)
              start-index (long (* start sample-rate))]
          (for [c (range (channels s))]
            (java.util.Arrays/copyOfRange (nth amps c)
                                          start-index
                                          (+ start-index (long (* dur sample-rate))))))))))

;;; Visualization

(defn visualize
  "Visualizes channel `c` (default 0) of `s` by plotting it on a graph."
  ([s] (visualize s 0))
  ([s c]
     (let [sample-rate 16000
           samples ^doubles (nth (amplitudes s sample-rate) c)
           n-samples (alength samples)
           indexes (range 0 n-samples (/ n-samples 4000))
           times (map #(/ (double %) sample-rate) indexes)]
       (incanter/view (charts/xy-plot
                       times
                       (map #(aget samples %) indexes)
                       :x-label "time (s)"
                       :y-label "amplitude"
                       :title (str "Amplitude for channel " c))))))
