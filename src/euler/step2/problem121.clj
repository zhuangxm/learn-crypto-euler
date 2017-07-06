(ns euler.step2.problem121
   (:require [clojure.math.combinatorics :as combo]))

(defn combinations
  [m n]
  (->>
   (combo/combinations (range 1 (inc m)) n)
   (map #(reduce * %))
   (reduce +)))

(defn answer [m]
  (let [mid (inc (int (/ m 2)))]
    (->>
     (range (inc (- m mid)))
     (map #(combinations m %))
     (reduce +)
     (/ (reduce * (range 2 (+ 2 m))))
     (long))))

(comment
 (answer 15))

