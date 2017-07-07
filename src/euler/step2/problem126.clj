(ns euler.step2.problem126
  (:require [euler.util :as util]))

;;C(x,y,z,n) = 2(x*y + y*z +  x*z)
;;             + 4(x+y+z + n -2)*(n-1)

(defn cubes
  "n >= 1"
  [x y z n]
  (+
    (* 2 (+ (* x y) (* x z) (* y z)))
    (* (dec n) 4 (+ x y z n -2))))

(defn cubes-seq
  [x y z]
  (->> (iterate inc 1)
       (map #(cubes x y z %))))

(defn cubes-seq-below
  [x y z n]
  (->> (cubes-seq x y z)
       (take-while #(<= % n))))

(defn cubes-x-y-z
  [n]
  (let [m (->> (range n)
               (map #(vector % (cubes % 1 1 1)))
               (take-while #(<= (last %) n))
               last
               first)]
    (when m
      (for [x (range 1 (inc m))
            y (range x (inc m)) :while (<= (cubes x y y 1) n)
            z (range y (inc m)) :while (<= (cubes x y z 1) n)]
        [x y z]))))

(defn cubes-frequency
  [max-cubes]
  (->> (cubes-x-y-z max-cubes)
       (mapcat #(apply cubes-seq-below (conj % max-cubes)))
       (frequencies)
       (sort-by first)))

(defn answer
  [solution-number max-cubes]
  (->> (cubes-frequency max-cubes)
       (filter #(= (last %) solution-number))
       first))

(comment
 (time (first (answer 1000 30000))))
