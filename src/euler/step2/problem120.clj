(ns euler.step2.problem120)

;;deduce by math formula
(defn r-max
  [a]
  (if (odd? a)
   (* a (dec a))
   (* a (- a 2))))

(defn answer
  [m n]
  (->> (range m (inc n))
       (map r-max)
       (reduce +)))

(comment (answer 3 1000))
