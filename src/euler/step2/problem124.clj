(ns euler.step2.problem124
  (:require [euler.problem95 :refer [factors]]
            [euler.util :as util]))

(defn all-factors
  [primes n]
  (reduce (fn [r v] (assoc r v (factors r primes v)))
          {} (range 2 (inc n))))

(defn compare-second-first
  [x y]
  (let [d (compare (last x) (last y))]
    (cond
      (= d 0) (compare (first x) (first y))
      :default d)))

(defn sorted-rad
  [n]
  (let [primes (->> (util/lazy-primes) (take-while #(< % n)))]
    (->> (all-factors primes n)
         (map (fn [[k v]] [k (reduce * (keys v))]))
         (cons [1 1])
         (sort-by identity compare-second-first))))

(defn answer
  [n index]
  (first (nth (sorted-rad n) (dec index))))

(time (answer 100000 10000))

;;(sorted-rad 100)
