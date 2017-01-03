(ns euler.problem66
  (:require [euler.problem64 :refer (convergents)]))

;;find solution for x^2 - D * y ^2 = 1

(defn sqrt
  [n]
  (let [s (int (Math/sqrt n))]
    (when (== n (* s s))
      s)))

(defn solution
  [d]
  (->> (convergents d)
    (some (fn [[h k]]
            (when (== (- (* h h) (* d k k)) 1)
              [d h])))))

(defn answer
  [n]
  (->> (range 0 (inc n))
    (filter (complement sqrt))
    (map solution)
    (sort-by second >)
    (first)))

;; (solution 61) 1766319049
(comment (answer 1000))