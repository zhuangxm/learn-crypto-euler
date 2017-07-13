(ns euler.step2.problem131
  (:require [euler.util :as util]))

(defn power3 
  [n]
  (* n n n))

(defn answer 
  [max-value]
  (->> (iterate inc 1)
       (map #(- (power3 (inc %))
                (power3 %)))
       (take-while #(< % max-value))
       (filter util/prime?)
       (count)))

(answer 1000000)
