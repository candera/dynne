(ns user
  "Holds utility functions for working at the REPL in this project"
  (:require [clojure.core.reducers :as r]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.repl :refer [doc pst]]
            [clojure.tools.namespace.repl :refer [refresh]]
            [dynne.operations :as ops]
            [dynne.sound :refer :all]
            [dynne.sampled-sound :as samp])
  (:import [javax.sound.sampled
            AudioFileFormat$Type
            AudioFormat
            AudioFormat$Encoding
            AudioInputStream
            AudioSystem]))

;; Debugging support
;;(alter-var-root #'*compiler-options* assoc :disable-locals-clearing true)
;; (require '[dynne.sound :refer :all])

(defn init
  [])

(defn go
  []
  (init))

;; (defn stop
;;   [])

(defn reset
  []
  ;; (stop)
  (refresh :after 'user/init))

(comment
  (->> (range 0 100)
       (r/map #(+ 3 %))
       (r/map (fn [n] [n (* 2 n)]))
       (reduce (fn ([x y]) (r/map + %1 %2)))
       (r/map #(/ % 3)))

  (->>
       (r/map #(+ 3 %) (range 0 100))
       (reduce +))

  (reduce + (r/map #(+ 3 %) [0 1 2 3])))

(defn oversample-all
  "Oversample all of s, for performance measurement purposes."
  [s]
  (let [inc-t (/ 1.0 44100)
        delta-t (/ inc-t 4)
        max-t (duration s)
        channels (channels s)]
    (loop [t 0.0]
      (when (< t max-t)
        (loop [c 0]
          ;;(sample s t)
          (when (< c channels)
            (oversample4 s t c delta-t)
            (recur (+ 1 c))))
        (recur (+ t inc-t))))))

(defn bench
  [dur]
  (let [path (str  "test" dur ".wav")]
   (when-not (.exists (io/file path))
     (save (->stereo (sinusoid dur 440)) path 44100))
   (-> ;;path
       ;;read-sound
    (sinusoid dur 440)
    ->stereo
       (fade-in 20)
       (fade-out 20)
       (mix (-> (square-wave 60 880)
                (timeshift (/ dur 2))
                (->stereo)))
       ;;(visualize)
       ;;(save (str "test-out" dur ".wav") 44100)
       oversample-all
       time
       )))
