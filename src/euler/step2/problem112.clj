(ns euler.step2.problem112
 (:require [clojure.math.combinatorics :as combo]))

;;every digital combinations only one or two non-bounty solutions.
;;for example
;; [3 5 3 4] only 5433 or 3345.
;; [ 5 5 5 5] only 5555

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
