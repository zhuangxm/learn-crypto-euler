(ns euler.step2.problem108
  (:require [euler.problem95 :refer [factors]]
            [clojure.math.combinatorics :as combo]
            [clojure.math.numeric-tower :as math]
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

(defn solution-count
  [count-seq]
  (reduce (fn [r v] (+ r (* v (dec (* 2 r)))))
          (inc (first count-seq))
          (rest count-seq)))

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
    (map #(math/expt (bigint %1) (bigint %2)) primes)
    (apply *)))

(defn max-count
  [ask-count]
  (->> (iterate inc 1)
    (map #(vector % (solution-count (repeat % 1))))
    (drop-while #(< (last %) ask-count))
    (ffirst)))

(defn find-primes-count
  [ask-count]
  (let [max-c (inc (max-count ask-count))]
    (->> max-c
      (range 1)
      (mapcat #(find-solution [%] max-c))
      (map #(vector % (solution-count %)))
      (filter #(> (last %) ask-count))
      (map (fn [[count-seq _]] (vector count-seq (calculate prime-10000 count-seq))))
      (sort-by last)
      first
      last)))

(defn cumulate
  [s primes]
  (->> s
    (map #(Math/pow %1 %2) primes)
    (apply +)))


(defn max-pow
   "return max n that small-p ^ n < bigger-p"
  [count-seq s limit primes ask-solution-count]
  (let [upper-limit (when (seq s) (+ (last s) (nth count-seq (dec (count s)))))
        upper-limit (or upper-limit limit)]
    (->> (range 0 (inc upper-limit))
      (map #(vector % (Math/pow (nth primes (count s)) %)))
      (take-while #(<= (last %) limit))
      (map #(conj s (first %)))
      (mapcat #(max-pow count-seq % (- limit (cumulate % primes)) primes ask-solution-count))
      (cons s))))

(defn move-forward
  [count-seq p primes ask-solution-count]
  (max-pow count-seq [] p primes ask-solution-count))

(defn smart-solution
  [primes count-seq ask-solution-count]
  (let [n (calculate primes count-seq)
        last-count (last count-seq)
        last-prime (nth primes (dec (count count-seq)))
        new-count-seq (if (== 1 last-count)
                        (drop-last count-seq)
                        (concat (drop-last count-seq) [(dec last-count)]))]
   (or
    (->>
     (move-forward count-seq last-prime primes ask-solution-count)
     (rest)
     (map #(concat % (repeat (- (count new-count-seq) (count %))
                             0)))
     (map #(map + % new-count-seq))
     (filter #(>= (solution-count %) ask-solution-count))
     (map #(vector % (calculate primes %)))
     (filter #(<= (last %) n))
     (sort-by last)
     first)
    [count-seq n])))

(defn brute-force-combinations
  [n]
  (->> (range (* 2 n) (inc (* n (inc n))))
     (map #(vector % (/ % (dec (/ % n)))))
     (filter #(integer? (last %)))
     (map (fn [[y x]] [y x (/ y x)]))))

(defn smart-answer
  [ask-count]
  (let [max-c (max-count ask-count)]
    (loop [count-seq (repeat max-c 1)
           n (calculate prime-10000 count-seq)]
      (let [[new-count-seq new-n] (smart-solution prime-10000 count-seq ask-count)]
         (prn ["smart-answer: " new-count-seq new-n])
         (if (< new-n n)
           (recur new-count-seq new-n)
           [new-count-seq n])))))


;;(find-primes-count 1000)
;;180180

;;(last (smart-answer 4000000))
;;9350130049860600
