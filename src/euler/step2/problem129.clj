(ns euler.step2.problem129
  (:require [euler.util :as util]
            [clojure.math.numeric-tower :as m]))

(defn count-of-one-slow 
  [n & [ones]]
  (->> (or ones (iterate #(inc (*' 10 %)) 1))
       (drop-while #(not= 0 (mod % n)))
       first
       (str)
       count))

(defn count-of-one-fast
  "using long divided remainder method
  https://www.khanacademy.org/math/arithmetic-home/multiply-divide/mult-digit-div-2/v/division-2"
  [n]
  (loop [l 1 k 1]
    (let [r (mod l n)]
      (if (zero? r) 
        k
        (recur (inc (* r 10)) (inc k))))))

(defn answer
  [n] 
  (->> (iterate inc n) 
       (remove #(or (zero? (mod % 2))
                    (zero? (mod % 5))))
       (map #(vector % (count-of-one-fast %)))
       (filter #(> (last %) n))
       ffirst))

(comment
 (time (answer 1000000)))
