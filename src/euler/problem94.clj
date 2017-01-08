(ns euler.problem94
  (:require [clojure.math.numeric-tower :as math]
            [euler.util :refer :all]))

;;analyze

;;https://en.wikipedia.org/wiki/Integer_triangle#Isosceles_Heronian_triangles
;;let u2 = u^2 v2 = v^2

;; a = 2(u2 - v2)
;; b = u2 + v2
;; c = u2 + c2

;;or
;; a = (u2 - v2)
;; b = (u2 + v2) /2
;; c = b

;; u > v and gcd(u v) == 1
;; because a = b + 1 or c - 1.
;; 2(u2)-2v2 = u2 + v2 +/- 1
;;u2-3v2 = +/-1, --> v = sqrt((u2 +/- 1) / 3)
;; let ask perimeter = p.
;; then a + b + c <=p
;; so 4u2 <= p  --->  u^2 <= p / 4  ----> u <= sqrt(p / 4)


(defn area
  [a b c]
  (let [p (/ (+ a b c) 2)
        a2 (* p (- p a) (- p b) (- p c))]
    [p a2 (Math/sqrt a2)]))

(defn u-v
  [u v coefficient]
  (let [u2 (* u u)
        v2 (* v v)]
    (when (and (== v (int v))
               (== 1 (math/gcd u (int v)))
               (divided-by? (+ u2 v2) coefficient))
      [(/ (+ u2 v2) coefficient)
       (/ (+ u2 v2) coefficient)
       (/ (* 2 (- u2 v2)) coefficient)
       u v])))

(defn find-v
  [u]
  (let [u2 (* u u)
        vi (Math/sqrt (/ (inc u2) 3))
        vd (Math/sqrt (/ (dec u2) 3))
        vi2 (Math/sqrt (/ (+ 2 u2) 3))
        vd2 (Math/sqrt (/ (- 2 u2) 3))]
    (or (u-v u vi 1)
        (u-v u vd 1)
        (u-v u vi2 2)
        (u-v u vd2 2))))

(defn answer
  [number]
  (->>
    (range 2 (inc (Math/sqrt (/ number 4))))
    (map find-v)
    (filter identity)
    (map #(apply + (take 3 %)))
    (filter #(<= % number))
    (apply +)))

