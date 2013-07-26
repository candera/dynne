(ns dynne.operations
  "Functions for manipulating operations on sounds, returning new operations."
  (:refer-clojure :exclude [compile])
  (:require [dynne.sound :as snd]
            [primitive-math :as p]))

;;; Input bindings

(defn input
  "Create an operation that just samples that sound
  without additional processing."
  [identifier]
  (let [s* (gensym "sound")]
   `{:bindings {~s* ~identifier}
     :duration (.duration ~s*)
     :channels (.channels ~s*)
     :amplitude (.amplitude ~s* ~'t ~'c)}))

;;; Composite operations

(defn gain
  "Create an operation that, given an operation and a gain, composes
  applying `gain` with that operation."
  [op gain]
  (let [amplitude (:amplitude op)]
    (assoc op
           :amplitude
           (list `p/* gain amplitude))))

(defn multiply
  "Returns an operation that composes `op1` and `op2` by multiplying
  their amplituds."
  [op1 op2]
  {:bindings (merge (:bindings op1) (:bindings op2))
   :duration `(min ~(:duration op1) ~(:duration op2))
   :channels (:channels op1)
   :amplitude `(p/* ~(:amplitude op1) ~(:amplitude op2))})

;;; Compilation

(defn input-bindings
  "Returns a vector of input bindings"
  [op-bindings inputs]
  (vec (mapcat (fn [[k v]]  [k (with-meta (list `get inputs v)
                                 {:tag 'dynne.sound.impl.ISound})])
               op-bindings)))

(defn compile
  "Returns a function that, given inputs bound per `inputs`, returns a
  reified sound that samples via `op`."
  [op]
  ;; TODO: Have a function that produes the op and one that evals that.
  (eval
   (let [input-sym (gensym "input")]
     `(fn [~input-sym]
        (let [~@(input-bindings (:bindings op) input-sym)]
          (snd/sound ~(:duration op)
                     (fn ~(with-meta (vector (with-meta 't {:tag 'double})
                                             (with-meta 'c {:tag 'long}))
                            {:tag 'double})
                       ~(:amplitude op))
                     ~(:channels op)))))))
