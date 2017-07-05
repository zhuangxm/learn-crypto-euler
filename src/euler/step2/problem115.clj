(ns euler.step2.problem115
  (:require [euler.step2.problem114 :as p114]))

(defn answer
  [n min-solutions]
  (->> (iterate inc n)
       (map #(vector % (p114/answer % n)))
       (filter #(>= (last %) min-solutions))
       first))

(time (answer 50 1000000))
