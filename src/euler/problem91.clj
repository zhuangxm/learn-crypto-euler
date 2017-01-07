(ns euler.problem91
  (:require [clojure.math.combinatorics :as combo]))

(defn power [n] (* n n))

(defn is-right-angled
  [[x1 y1] [x2 y2]]
  #_(prn [x1 y1] " -- " [x2 y2])
  (let [d1 (+ (power x1) (power y1))
        d2 (+ (power x2) (power y2))
        d3 (+ (power (- x2 x1)) (power (- y2 y1)))
        [p1 p2 p3] (sort [d1 d2 d3])]
    (== (+ p1 p2) p3)))

(defn points
  [n]
  (for [a (range (inc n)) b (range (inc n))
        :when (or (not= 0 a) (not= 0 b))]
    [a b]))

(defn answer
  [n]
  (let [ps (points n)]
    (->> (combo/combinations ps 2)
      (filter #(apply is-right-angled %))
      count)))