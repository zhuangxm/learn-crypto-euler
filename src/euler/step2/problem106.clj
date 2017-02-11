(ns euler.step2.problem106
  (:require [clojure.math.combinatorics :as combo]))

(defn intersect?
  [[s1 s2]]
  (let [different (map - s1 s2)]
    (or (every? neg? different)
        (every? pos? different))))

(defn need-check
  [pairs]
  (->> (combo/combinations pairs 2)
    (remove (fn [[c1 c2]] (seq (clojure.set/intersection (set c1) (set c2)))))
    (remove intersect?)))

(defn need-check-count [s]
  (let [s (sort s)
        c (count s)
        h (quot c 2)]
    (->> (range 2 (inc h))
      (map #(combo/combinations s %))
      (map need-check)
      (map count)
      (apply +))))

(defn next-near-optimums
  [last-optimum]
  (let [middle-index (quot (count last-optimum) 2)
        middle (nth last-optimum middle-index)]
    (cons middle (map #(+ middle %) last-optimum))))

(defn optimums
  [n]
  (loop [i 1 ps [1]]
    (if (== i n)
      ps
      (recur (inc i) (next-near-optimums ps)))))

(defn answer
  []
  ;;generate a n=12 optimums
  (need-check-count (optimums 12)))

#_(answer)

;;21384
