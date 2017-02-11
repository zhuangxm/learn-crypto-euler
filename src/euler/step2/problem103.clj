(ns euler.step2.problem103
  (:require [clojure.math.combinatorics :as combo]))

(def optimums [[1]
               [1 2]
               [2 3 4]
               [3 5 6 7]
               [6 9 11 12 13]
               [11 18 19 20 22 25]])

(defn next-near-optimums
  [n]
  (let [last-optimum (nth optimums (- n 2))
        middle-index (quot (count last-optimum) 2)
        middle (nth last-optimum middle-index)]
    (cons middle (map #(+ middle %) last-optimum))))

(defn min-a1
  [n]
  (max 1 (dec n)))

(defn min-seq
  [n]
  (let [m (min-a1 n)]
    (range m (+ m n))))

(defn more-elements-bigger? [s]
  (let [c (count s)
        h (if (odd? c)
            (inc (quot c 2))
            (/ c 2))]
    (loop [n h]
      (if (== n 1)
        true
        (if (> (apply + (take n s))
              (apply + (take-last (dec n) s)))
          (recur (dec n))
          false)))))

(defn not-equals? [s]
  (let [c (count s)
        h (quot c 2)]
    (->> (range 2 (inc h))
      (map #(combo/combinations s %))
      (map (fn [pairs] (map #(apply + %) pairs)))
      (every? #(apply distinct? %)))))

(defn optimum?
  [s]
  (and (more-elements-bigger? s)
      (not-equals? s)))

(defn combinations [n]
  (let [candidate (next-near-optimums n)]
    (->> candidate
      (map #(range (- % 3) (+ % 4)))
      (apply combo/cartesian-product)
      (map sort)
      (filter #(apply distinct? %))
      (filter optimum?))))

(defn answer []
  (->> (combinations 7)
      (map #(cons (apply + %) %))
      (sort-by first)
      first
      (rest)
      (apply str)))

#_(answer)
;;20313839404245

