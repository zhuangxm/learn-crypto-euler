(ns euler.stpe2.problem111
  (:require [euler.util :as util]
            [clojure.math.combinatorics :as combo]))

(defonce prime-100000 (util/primes 100000))
(defonce prime-100000-set (set prime-100000))

(defn prime?
  [n]
  "check if n is prime? n < 10,000,000,000"
  (or (prime-100000-set n)
      (every? #(not= 0 (mod n %)) prime-100000)))

(def digitals [0 1 2 3 4 5 6 7 8 9])

(defn numbers
  [total-digits repeat-digits digit]
  (let [ds (repeat repeat-digits digit)
        n (- total-digits repeat-digits)
        selections (combo/combinations (->> digitals
                                        (remove #{digit})
                                        (mapcat #(repeat n %)))
                                      n)]
    (mapcat
     #(combo/permutations (concat ds %))
     selections)))

(defn primes
  ([m d]
   (->> (range 1 (inc m))
        (reverse)
        (map #(primes m % d))
        (filter seq)
        first))
  ([m n d]
   (let [min-num (Math/pow 10 (dec m))]
     (->> (numbers m n d)
          (map #(apply str %))
          (map #(Long/parseLong %))
          (filter #(> % min-num))
          (filter prime?)))))

(defn sum
  [m d]
  (reduce + (primes m d)))

(defn answer
  [m]
  (->> digitals
       (mapcat #(primes m %))
       (apply +)))

(comment
  (answer 10))
