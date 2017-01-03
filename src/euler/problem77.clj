(ns euler.problem77
  (:require [euler.util :refer :all]))

(declare mem-solutions)

(defn append-solution
  [n p ss]
  (let [rs (map #(merge-with + {p 1} %) ss)]
    (if (prime? (- n p))
      (cons (merge-with + {p 1} {(- n p) 1}) rs)
      rs)))

(defn find-solutions
  [n]
  (cond
    (< n 2) nil
    (or (== n 2) (== n 3)) [{n 1}]
    :else (->> prime-10000
            (filter #(<= % (/ n 2)))
            (mapcat #(append-solution n % (mem-solutions (- n %))))
            distinct)))

;;very important to accelerate the calculation
(def mem-solutions (memoize find-solutions))

(defn solution-count
  [n]
  (count (find-solutions n)))

(defn answer
  [ask-count]
  (->> (iterate inc 2)
    (map #(vector % (solution-count %)))
    (some #(when (> (second %) ask-count) %))))