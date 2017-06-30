(ns euler.step2.problem112
 (:require [clojure.math.combinatorics :as combo]))

(defn cs
  [digits n]
  (let [s (mapcat #(repeat n %) digits)
        s1 (mapcat #(repeat n %) (rest digits))
        d (combo/count-combinations s n)
        d1 (combo/count-combinations s1 n)]
    (if (= n 1)
      d
      (+ d d1 -10))))


(defn answer [n]
  (->> (range 1 (inc n))
       (map #(cs [0 1 2 3 4 5 6 7 8 9] %))
       (reduce +)
       (dec)))

(comment
 (answer 100))
