(ns euler.problem78
  (:require [euler.util :refer :all]))

(defn pentagon
  [k]
  (/ (- (* 3 k k) k) 2) )

(def pentagonal-nums
  (->> (iterate inc 1)
    (mapcat #(vector % (- %)))
    (map #(vector % (pentagon %)))))

(declare pn)

(defn sub-pn
  [n v-cs [k v]]
  (let [p (nth v-cs (- n v) nil)]
    (if (odd? k) p (- p))))

(defn sum-up
  [n v-cs]
  (->> pentagonal-nums
    (take-while #(>= n (second %)))
    (reduce #(+ %1 (sub-pn n v-cs %2)) (bigint 0))))

(defn sum-up2
  [n v-cs]
  #_(prn "n: " n " v-cs: " v-cs)
  (reduce
   (fn [r v]
     #_(prn r " " v)
     (if (>= n (second v))
       (+ r (sub-pn n v-cs v))
       (reduced r)))
   (bigint 0) pentagonal-nums))

(defn pn-
  [n v-cs]
  (when (zero? (mod n 1000)) (prn n))
  (cond
    (or (== n 1) (== n 0)) 1
    (< n 0) 0
    :else (sum-up2 n v-cs)))

(defn pn
  [n]
  (->> (range 0 (inc n))
    (reduce (fn [cs v] (conj cs (pn- v cs))) [])
    last))

(defn pn-version2
  [n]
  (last (nth (pn-seq) n)))

(defn pn-seq
  []
  (let [cs (volatile! [])]
    (iterate
     (fn [[index v]]
       (let [n (pn- index @cs)]
         (do
           #_(prn n  " " @cs)
           (vswap! cs conj n)
           [(inc index) n])))
     [0 1])))

(defn answer
  [divisor]
  (->> (pn-seq)
    (some (fn [[index v]] (when (divied-by? v divisor) index)))))

#_(answer 1000000)
;;TODO too slow show optimize
;;55374

;;memoize version:  time: 138,136ms
;;vector version:   time:  8,260ms