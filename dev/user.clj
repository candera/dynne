(ns user
  "Holds utility functions for working at the REPL in this project"
  (:require [clojure.core.reducers :as r]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]
            [clojure.repl :refer [doc pst]]
            [clojure.tools.namespace.repl :refer [refresh]]
            [dynne.sound :refer :all]
            [dynne.sound.impl :as impl])
  (:import [javax.sound.sampled
            AudioFileFormat$Type
            AudioFormat
            AudioFormat$Encoding
            AudioInputStream
            AudioSystem]))

(defn init
  [])

(defn go
  []
  (init))

(defn stop
  [])

(defn reset
  []
  (stop)
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
