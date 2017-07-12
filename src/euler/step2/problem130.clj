(ns euler.step2.problem130
  (:require [euler.util :as util]))

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
  (->> (iterate inc 1) 
       (remove #(or (zero? (mod % 2))
                    (zero? (mod % 5))))
       (remove util/prime?)
       (map #(vector % (count-of-one-fast %)))
       (filter #(zero? (mod (dec (first %)) (last %))))
       (take n)
       (map first)
       (apply +)))

(comment
 (time (answer 25)))
