(ns euler.problem68
  (:require [clojure.math.combinatorics :as combo]))

(defn divided-by?
  [n d]
  (== 0 (mod n d)))

(defn inner-circle
 "inner-circle combinations
  c: inner cirlce Number
  ns: all numbers can be used"
  [c numbers]
  (->>
    (combo/combinations numbers c)
    (filter #(divided-by? (apply + %) c))))

(defn add-to-chain
  "put a in different position of circle numbers"
  [numbers a]
  (map #(concat (take % numbers) [a] (drop % numbers))
       (range 1 (inc (count numbers)))))

(defn circular-permutation
  [numbers]
  (reduce (fn [r n] (mapcat #(add-to-chain % n) r))
          [(take 2 numbers)] (drop 2 numbers)))

(defn find-outsides
  [inners all-numbers]
  (let [c (count inners)
        sum (apply + (concat inners all-numbers))
        q (/ sum c)
        outsides (for [a (range c)
                       :let [n1 (nth inners a)
                             n2 (nth inners (mod (inc a) c))] ]
                   [(- q (+ n1 n2)) n1 n2])]
    (when (= (set all-numbers) (set (concat inners (map first outsides))))
      outsides)))

(defn ->sort-circle
  [circle]
  (let [lead (apply min (map first circle))
        [f b] (split-with #(not= (first %) lead) circle)]
    (concat b f)))

(defn solutions
  [number]
  (let [numbers (range 1 (inc number))
        circle-number (/ number 2)
        inners (inner-circle circle-number numbers)]
    (->> inners
      (mapcat circular-permutation)
      (map #(find-outsides % numbers))
      (filter identity)
      (map ->sort-circle)
      (map #(apply concat %))
      (map #(apply str %))
      (filter #(not= (.length %) 17)) ;;filter string of length of 17
      sort
      last)))