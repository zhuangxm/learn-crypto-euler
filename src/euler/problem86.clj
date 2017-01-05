(ns euler.problem86
  (:require [euler.problem75 :refer [mnk->abc find-all-n]]))

  (defn all-k-mn
    "find all [[1..k] m n] triangles
     the short right side <= max-side
     and long right side <= 2(max-side)"
    [max-side [m n]]
    (let [[a b] (sort (mnk->abc [m n 1]))]
      (->> (iterate inc 1)
        (map (fn [k] [(* k a) (* k b)]) )
        (take-while (fn [[s1 s2]] (and (<= s1 max-side)
                                       (<= s2 (* 2 max-side))))))))

(defn tri->cuboid
  "divide one side of trigle to two integer numbers
      that all less than max-side, [a b] -> [m n x]
      max-side : the max of m n x
      a b: the two right sides of the right triangles
      m n x : three side of a cuboid "
  [max-side [a b]]
  (let [bs (->> (range 1 (inc (int (/ b 2))))
             (map #(vector % (- b %)))
             (filter (fn [[b1 b2]] (and (<= b1 a max-side) (<= b2 a max-side))))
             (map #(cons a %))
             (map sort))
        as (when (<= b max-side)
             (->> (range 1 (inc (int (/ a 2))))
               (map #(vector % (- a %)))
               (map #(cons b %))
               (map sort)))]
    (concat bs as)))

(defn all-mn
  [max-side m]
  (->> (find-all-n m)
    (mapcat (partial all-k-mn max-side))))

(defn all-right-triangles
  [max-side]
  (->> (iterate inc 2)
    (map #(all-mn max-side %))
    (take-while seq)
    (mapcat identity)
    (mapcat #(tri->cuboid max-side %))
    count))

(defn answer
  [ask-solutions]
  (->> (iterate inc 2)
    (map #(do (when (== 0 (mod % 20)) (prn %)) %))
    (map #(vector % (all-right-triangles %)))
    (some #(when (>= (second %) ask-solutions) %))))


#_(answer 1000000)

;;1818 TODO take too long to solve. need to optimize