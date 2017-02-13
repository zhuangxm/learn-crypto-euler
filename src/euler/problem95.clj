(ns euler.problem95
  (:require [euler.util :refer :all]))

;;reference
;; sum of the factors
;;http://math.stackexchange.com/questions/163245/finding-sum-of-factors-of-a-number-using-prime-factorization

(defn factors
  "find all prime factors"
  [fs primes n]
  (let [[p1 p2] (->> primes
                 (filter #(< % (inc (nearest-sqrt n))))
                 (some #(when (divided-by? n %) [% (quot n %)])))]
    (if p1
      (cond
        (= p1 p2) (merge {p1 2})
        (get fs p2) (merge-with + {p1 1} (get fs p2))
        :else {p1 1 p2 1})
      {n 1})))

(defn all-factors
  [primes n]
  (reduce (fn [r v] (assoc r v (factors r primes v)))
          {} (range 2 (inc n))))

(defn sum-factor
  [n factors]
  (int (- (reduce (fn [r [k v]] (* r (/ (dec (Math/pow k (inc v)))
                                        (dec k))))
              1 factors)
          n)))

(defn sum-factors
  [primes n]
  (let [fs (all-factors primes n)]
    (into {} (map #(vector % (sum-factor % (get fs %)))
                (range 2 (inc n))))))

(defn amicable-chain
  [sums n]
  (loop [cs #{n} a (get sums n)]
    (when a
      (if (= a n)
          cs
          (when-not (cs a)
            (recur (conj cs a) (get sums a)))))))

(defn filter-impossible
 [sums]
 (->> sums
   (filter (fn [[k v]] (and (not= k v) (get sums v))))
   (into {})))

(defn loop-impossible
 [sums]
 (loop [s sums]
   (let [after (filter-impossible s)]
     (if (= (count s) (count after))
       s
       (recur after)))))

(defn amicable-chains
  [primes n]
  (let [sums (sum-factors primes n)
        sums (loop-impossible sums)]
    (->> sums
      (map #(amicable-chain sums (first %)))
      (filter identity)
      (distinct)
      (sort-by count)
      (reverse)
      (first)
      (sort)
      first)))

#_(amicable-chains prime-10000 1000000)