(ns euler.step2.problem123
  (:require [euler.util :as util]))

(defn r
  [a n]
  (if (odd? n)
   (* a (* 2 n))
   2))

(defn answer
  [ask-value]
  (->> (util/lazy-primes)
       (map-indexed #(vector (inc %1) %2 (r %2 (inc %1))))
       (filter #(> (last %) ask-value))
       first))

(time (answer (reduce * (repeat 10 10))))
