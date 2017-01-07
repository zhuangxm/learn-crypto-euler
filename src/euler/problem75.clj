(ns euler.problem75
  (:require [euler.util :refer :all]
            [clojure.math.numeric-tower :refer :all]))

;;https://en.wikipedia.org/wiki/Pythagorean_triple

;; a = k(m^2 - n^2) ; b = k(2mn) ; c = k(m^2 + n^2)
;; k positive m > n , and m.n coprime, and m,n are not both odd.
;; p = (a + b + c) = 2k (m^2 + mn)
;; p must be even sqrt(p/2k) / 2  <  m <  sqrt(p/2k)

;;this solution is very slow when p is big
;; p --> k --> m --> n

(defn find-ks
  [n]
  (->> (range 1 (inc (nearest-sqrt n)))
    (mapcat #(when (divided-by? n %) [% (/ n %)]))))

(defn find-n
  [p k m]
  (let [n  (-> p (/ 2 k) (- (* m m)) (/ m))]
    (when (and (> m n)
               (> n 0)
               (== (int n) n))
      [m n k])))

(defn find-mn
  [p k]
  (let [co (/ p 2 k)
        max-m (nearest-sqrt co)]
    (->> (range (max 2 (int (/ max-m 2))) (inc max-m))
      (map #(find-n p k %))
      (filter identity))))

(defn mnk->abc
  [[m n k]]
  [(* k (- (* m m) (* n n)))
   (* k 2 m n)
   (* k (+ (* m m) (* n n)))])

(defn convert-and-combine
  [rs coll]
  (->> coll
    (map mnk->abc)
    (map set)
    set
    (clojure.set/union rs)))

(defn right-triangles
  [p & [ask-count]]
  (when (== 0 (mod p 30000)) (prn "try p: " p))
  (when (even? p)
    (->> (find-ks (/ p 2))
      (reduce (fn [r v]
                (if (and (pos? (or ask-count 0))
                         (>= (count r) ask-count))
                  (reduced r)
                  (convert-and-combine r (find-mn p v))))
              #{}))))

(defn answer-slow
  [p & [ask-count]]
  (->> (range 3 (inc p))
    (map #(right-triangles % ask-count))
    (filter seq)
    (filter #(== 1 (count %)))
    count))

;;-------------------another solution -------------------------------------------
;; find all m n k and generated triangles
;; another direction
;; this solution is better than previous one.
;; m n k --> p

(defn find-all-n
  [m]
  (for [n (range (if (even? m) 1 2) m 2)
        :when (== 1 (gcd m n))]
    [m n]))

(defn all-triangles
  [p m n]
  (let [tri (mnk->abc [m n 1])
        s (apply + tri)
        max-k (int (/ p s))]
    (into {} (map #(vector (* s %) [(mnk->abc [m n %])]) (range 1 (inc max-k))))))

(defn all-right-triangles
  [max-p]
  (let [max-m (nearest-sqrt (int (/ max-p 2)))]
    (reduce (fn [r v] (merge-with concat r (apply all-triangles max-p v)))
            {} (mapcat find-all-n (range 2 (inc max-m))))))

(defn answer
  [max-p]
  (->> max-p
    (all-right-triangles)
    (map (comp count second))
    (filter #(== 1 %))
    count))

#_(answer 1500000)
;;161667