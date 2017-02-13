(ns euler.step2.problem108
  (:require [euler.problem95 :refer [factors]]
            [clojure.math.combinatorics :as combo]
            [euler.util :refer :all]))

;;http://www.cut-the-knot.org/arithmetic/ShortEquationInReciprocals.shtml#solution
;; x=km(m+n), y=kn(m+n), z=kmn,

;;where the three parameters k,m,n are positive integers.

;; if the prime-factors of a number is {p-a count-a p-b count-c ...}
;;then the solution counts of the number
;; (count-a * count-b) + (count-a + 1) * (count-b * 1) - 1
;; so find count-a and count-b or count-a count-b count-c satisfy the requirement

(defn lazy-factors
  [primes]
  (reductions (fn [r v] (assoc r v (factors r primes v)))
          {} (iterate inc 2)))

(defn product-count
   "combinations like [[a a] [b b b] [c c]]
    then count-seq = [2 3 2]
    return count of [[a b] [a b b] [a b b b] [a b c] [a b c c] ...]
   "
  [count-seq]
  (let [t (->> count-seq
            (map inc)
            (apply *))]
    (->> count-seq
      (map #(dec (/ t (inc %))))
      (map #(* %1 %2) count-seq)
      (apply +))))

;; TODO this function is incorrect,
;; it should have a simple method to get the solution count
(defn solution-count
  [count-seq]
  (let [c (apply + count-seq)]
    (->> count-seq
      (map inc)
      (apply *)
      (+ (apply * count-seq))
      (+ (product-count count-seq))
      (+ (- (/ (* c (dec c))
               2)))
      (+ (count count-seq))
      (+ (- 4)))))

;;this is some kind of brute force method to get solution count
(defn simulate-solution-count
  [count-seq]
  (->> count-seq
    (map-indexed #(repeat %2 %1))
    (map combo/subsets)
    (apply combo/cartesian-product)
    (map #(filter seq %))
    (mapcat #(combo/partitions % :min 2 :max 2))
    (count)
    (+ (apply * (map inc count-seq)))))

(defn find-solution
  [count-seq ask-count]
  (let [c (apply + count-seq)
        cs (->> (range 1 (inc (last count-seq)))
                (take-while #(<= (+ c %) ask-count))
                (map #(conj count-seq %)))]
    (concat cs
            (->> cs
              (mapcat #(find-solution % ask-count))))))

(defn calculate [primes count-seq]
  (->> count-seq
    (map #(Math/pow %1 %2) primes)
    (apply *)))

(defn max-count
  [ask-count]
  (->> (iterate inc 1)
    (map #(vector % (Math/pow 2 %)))
    (drop-while #(< (last %) ask-count))
    (ffirst)))

(defn find-prims-count
  [ask-count]
  (let [max-c (inc (max-count ask-count))]
    (->> max-c
      (range 1)
      (mapcat #(find-solution [%] max-c))
      (map #(vector % (simulate-solution-count %)))
      (filter seq)
      (filter #(> (last %) ask-count))
      (map (fn [[count-seq _]] (vector count-seq (calculate prime-10000 count-seq))))
      (sort-by last)
      first
      last)))

(defn brute-force-combinations
  [n]
  (->> (range (* 2 n) (inc (* n (inc n))))
     (map #(vector % (/ % (dec (/ % n)))))
     (filter #(integer? (last %)))
     (map (fn [[y x]] [y x (/ y x)]))))