(ns euler.step2.problem127
  (:require [euler.util :as util]))

(defn all-factors
  [primes n]
  (reduce (fn [r p]
            (->> (iterate inc 1)
                 (map #(* % p))
                 (take-while #(<= % n))
                 (reduce #(update %1 %2 (fnil conj #{}) p)
                         r)))
          {} primes))

(defn rads
  [n]
  (let [primes (->> (util/lazy-primes) (take-while #(< % n)))]
    (->> (assoc (all-factors primes n) 1 #{1})
         (map (fn [[k v]] [k (set v) (reduce * v)])))))

(defn rads-of
  [n]
  (->> (rads n)
       (map #(vector (first %) (rest %)))
       (into {})))

(defn all-c-hit
  [rads sorted-rads c]
  (let [rad-c (get rads c)
        rad-c-set (first rad-c)
        product-rad-c (last rad-c)
        min-ab (/ c product-rad-c)
        max-a (int (/ c 2))]
    (for [a (->> sorted-rads (map first) (filter #(< % max-a)))
          :let [b (- c a)
                rad-a (get rads a)
                p-a (last rad-a)
                rad-b (get rads b)]
          :while (< (* p-a product-rad-c)
                    c)
          :when (and (< (* p-a (last rad-b) product-rad-c)
                        c)
                     (empty? (clojure.set/intersection (first rad-a) (first rad-b) (first rad-c))))]

      [a b (last rad-a) (last rad-b) (last rad-c) c])))

(defn compare-rad
  [x y]
  (let [d (compare (last (last x)) (last (last y)))]
    (cond
      (= d 0) (compare (first x) (first y))
      :default d)))

(defn solutions
  [n]
  (let [rs (rads-of (inc n))
        sorted-rs (sort-by identity compare-rad rs)]
    (->> (range 3 (inc n))
         (mapcat #(all-c-hit rs sorted-rs %)))))

(defn answer
  [n]
  (->> (solutions n)
       (map last)
       (reduce +)))

(comment
 (time (answer 120000)))
