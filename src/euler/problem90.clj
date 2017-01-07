(ns euler.problem90
  (:require [clojure.math.combinatorics :as combo]))

(def squares (map (fn [n] [(quot (* n n) 10) (mod (* n n) 10)])
                 (range 1 10)))

(def first-sets (set (map first squares)))
(def second-sets (set (map second squares)))

(defn all-combinations
  []
  (map set (combo/combinations (range 10) 6)))

(defn handle-69
  [s]
  (if (seq (clojure.set/intersection s #{6 9}))
    (conj s 6 9)
    s))

(defn ok?
  [s1 s2 [n1 n2]]
  (let [s1 (handle-69 s1)
        s2 (handle-69 s2)]
    (or (and (s1 n1) (s2 n2))
        (and (s1 n2) (s2 n1)))))

(defn all-ok?
  [s1 s2]
  (every? #(ok? s1 s2 %) squares))

(defn all-cubes
  []
  (let [s (all-combinations)]
    (for [a s b s]
      [a b])))

(defn answer
  []
  (->>
    (all-cubes)
    (filter #(apply all-ok? %))
    (map set)
    (set)
    count))

#_(answer)

;;1217 time: 532ms

