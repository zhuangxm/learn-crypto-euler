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
      (take-while (fn [[s1 s2]]
                    (and (<= s1 max-side)
                         (<= s2 (* 2 max-side))))))))

(defn all-abc
  [max-side m]
  (->> (find-all-n m)
    (mapcat (partial all-k-mn max-side))))

(defn tri->cuboid
  "divide one side of trigle to two integer numbers
   that all less than max-side, [a b] -> [m n x]
   max-side : the max of m n x
   a b m: the two right sides of the right triangles
   return s1 s2 s3] : three side of a cuboid and s1 < s2 < s3"
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

(defn all-right-triangles
  [max-side & [from-m]]
  (->> (iterate inc (or from-m 2))
    (map #(all-abc max-side %))
    (take-while seq)
    (mapcat identity)
    (mapcat #(tri->cuboid max-side %))))

(defn analyze-triangles
  [triangles]
  (->> triangles
    (group-by last)
    (sort-by first)
    (map (fn [[k v]] [k (count v)]))
    (reductions (fn [[rk rv] [k v]] [k (+ rv v)])
                [2 0])))

(defn all-right-triangles-count
  [max-side]
  (count (all-right-triangles max-side)))

(defn answer
  [ask-solutions]
  (->> (iterate #(+ 1000 %) 1000)
    (map #(vector % (all-right-triangles %)))
    (some #(when (>= (count (second %)) ask-solutions) (second %)))
    (analyze-triangles)
    (some #(when (>= (second %) ask-solutions) %))))

#_(answer 1000000)

;;1818 TODO take too long to solve. need to optimize

;; time after optimization 3,308ms
;;optimization process
;; max-side means the max length of each side of the cuboid
;; 1. find the first one max-side that has as solution bigger thant ..
;; 2. the important things is the first step is not find the exact max-size
;; but any number that satifies this condition , so we can choose a bigger number or
;; using a big step (here we choose 1000 a step))
;; 3. anlayze that solution and get the solutions for each max-sides below
;; 4. choose the exactly answer.

;; for each-example :
;; first (all-right-triangles 1000) and then (all-right-triangles 2000)
;; (> (all-right-triangles 2000) 1000000)
;; then analyze the result (all-right-triangles 2000),
;;  get ([2 0] [3 2] [4 3] [6 6] [8 10] [9 14] ...[1817 (< n 1000000)] [1818 1000457])
;;  [2 0] the 2 means max-sides, 0 means solutions of max sides 2
;;  finally find the result that we want [1818 1000457]

