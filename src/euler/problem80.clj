(ns euler.problem80
  (:require [euler.util :refer :all]))

(defn irration-sqrt
  [n]
  (let [big-n (bigdec n)]
    (loop [x (bigdec (nearest-sqrt n))]
      (let [nx (with-precision 102 (/ (+ x (/ big-n x)) 2))]
        (if (not= nx x)
          (recur nx)
          (str x))))))

(defn calculate-digit-sub
  [s]
  (->> (clojure.string/split s #"\.")
    (apply concat)
    (take 100)
    (map #(Integer/parseInt (str %)))
    (apply + )))

(defn answer
  [n]
  (->>
    (range 1 (inc n))
    (filter (complement can-sqrt?))
    (map irration-sqrt)
    (map calculate-digit-sub)
    (apply +)))

#_(answer 100)

;;40886