(ns euler.step2.problem118
  (:require [euler.util :as util]
            [clojure.math.combinatorics :as combo]))

(defonce prime-34000 (util/primes 34000))
(defonce prime-34000-set (set prime-34000))

(defn combos
  []
  (->>
    (combo/permutations [1 2 3 4 5 6 7 8 9])
    (filter #(not (#{2 4 6 8} (last %))))))

(defn prime?
  [n]
  "check if n is prime? n < 1,000,000,000"
  (or (prime-34000-set n)
      (and (> n 34000)
           (->> prime-34000
                (take-while #(<= % (inc (util/nearest-sqrt n))))
                (every? #(not= 0 (mod n %)))))))

(defn expand
  [s colls]
  (if (seq colls)
    (map #(cons s %) colls)
    [[s]]))

(defn primes-combination
  [min-p coll]
  (if-not (seq coll)
    [[0]]
    (let [c (count coll)]
      (->> (range 1 (inc c))
           (map #(split-at % coll))
           (map #(vector (Integer/parseInt (apply str (first %)))
                         (last %)))
           (filter #(and (> (first %) min-p) (prime? (first %))))
           ;;(map #(do (prn %) %))
           (mapcat #(expand (first %) (primes-combination (first %) (last %))))))))

;;(primes-combination [2 5 4 7 8 9 6 3 1])

(defn answer
  []
  (->> (combos)
       (mapcat #(primes-combination 0 %))
       (filter #(= 0 (last %)))
       ;;(map set)
       ;;distinct
       (count)))

(comment
 (time (answer)))
