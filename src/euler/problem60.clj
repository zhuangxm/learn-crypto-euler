(ns euler.problem60
  (:require [euler.util :as util :refer [primes prime?]]))

(defn split-p
  [p]
  (when (>= p 10)
    (let [s (str p)
          l (.length s)]
      (for [a (range 1 l)]
        [(quot p (Math/pow 10 a))
         (mod p (Math/pow 10 a))]))))

(defn combine-p?
  [p1 p2]
  (and (prime? (Integer/parseInt (str p1 p2)))
       (prime? (Integer/parseInt (str p2 p1)))))

(defn all-combines
  [ps]
  (filter identity
          (for [p1 ps p2 ps :when (< p1 p2)]
            (when (combine-p? p1 p2)
              #{p1 p2}))))

(defn all-fit?
  [sub-ps p combines-2-set]
  (and (not (sub-ps p))
       (every? #(combines-2-set #{p %}) sub-ps)))

(defn fill-another-p
  [sub-ps ps combines-2-set]
  (filter identity
          (for [p ps]
            (when (all-fit? sub-ps p combines-2-set)
              (conj sub-ps p)))))

(defn solutions
  [n ask-count]
  (let [ps (primes n)
        combines-2-set (set (all-combines ps))]
    (loop [pss combines-2-set
           c ask-count]
      (if (>= c 2)
        (recur (->> pss
                 (mapcat #(fill-another-p % ps combines-2-set))
                 (filter identity)
                 distinct)
          (dec c))
        pss))))

;;answer (solutions 9000 4)
;;#{5701 13 6733 5197 8389}
(comment
 (apply + (first (solutions 9000 4))))
