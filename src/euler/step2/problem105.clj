(ns euler.problem105
  (:require [euler.step2.problem103 :refer [optimum?]]))

(def sets-file-name "resources/p105_sets.txt")

(defn line->sets
  [line]
  (->> (clojure.string/split line #",")
    (map #(Integer/parseInt %))))

(defn load-sets []
  (->> (slurp sets-file-name)
    (clojure.string/split-lines)
    (map line->sets)))

(defn answer []
  (->> (load-sets)
    (map sort)
    (filter optimum?)
    (map #(apply + %))
    (apply +)))

