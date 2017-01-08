(ns euler.problem93
  (:require [clojure.math.combinatorics :as combo]))

;;in order to avoid brackets/parentheses, we invent
;; two operations reverse-minus and reverse-div
;; so every operation can be written without brackets and calculate form front to end.
;; like 4 / (3 - 2 + 1) ====  3 - 2 + 1  reverse-div 4
;; 3 - 4 * 5 * 6  ====   4 * 5 *6 reverse-minus 3
;; the expression calculate from front to end. ignore the priority of arithmetic operation

(defn reverse-div
  [a b]
  (if (not= a 0)
    (/ b a)
    -1000000000000000))

(defn reverse-minus
  [a b]
  (- b a))

(defn operations
  []
  (combo/selections [+ - * / reverse-minus reverse-div] 3))

(defn cal
  [[f1 f2 f3] [a b c d]]
  #_(prn "cal: " [a b c d])
  (-> a
    (f1 b)
    (f2 c)
    (f3 d)))

(defn cal-permutation
  [fs numbers]
  #_(prn fs " " numbers)
  (map #(cal fs %) (combo/permutations numbers)))

(defn max-consecutive
  [coll]
  (some #(when-not (coll %) (dec %)) (iterate inc 1)))

(defn cal-all
  [[a b c d]]
  (->> (operations)
    (mapcat #(cal-permutation % [a b c d]))
    (filter (fn [v] (and (> v 0) (== v (int v)))))
    set
    max-consecutive))


(defn answer
  []
  (let [numbers (combo/combinations (range 1 10) 4)]
    (->> numbers
      (map #(vector % (cal-all %)) )
      (sort-by second)
      (last))))

;;time 3,411ms
;;[(1 2 5 8) 51]