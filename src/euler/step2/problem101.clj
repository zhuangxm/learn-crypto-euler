(ns euler.step2.problem101
  (:require [clojure.core.matrix :as matrix]))

;;using matrix to solve the equation.

(matrix/set-current-implementation :vectorz)

(defn poly
  [n]
  (->> (range 11)
    (map #(Math/pow n %))
    (map #(* %1 %2) (iterate - 1))
    (apply +)))

(defn n-k
  "return n^0 ... n^k"
  [n k]
  (map #(Math/pow n %) (range k)))

(defn k-co-efficient
  [num-seq k]
  (->>
    (range 1 (inc k))
    (map #(n-k % k))
    (matrix/inverse)
    (matrix/mul (take k num-seq))
    (map #(apply + %))))

(defn calculate
  [n coefficients]
  (->> (range (count coefficients))
    (map #(Math/pow n %))
    (map #(* %1 %2) coefficients)
    (apply +)))


(defn answer
  [num-seq]
  (let [cos (->> (range 1 (inc (count num-seq)))
              (map #(k-co-efficient num-seq %)))]
    (prn cos)
    (->> cos
      (map #(calculate (inc (count %)) %))
      (map #(Math/round %))
      (apply +))))

#_(answer (map poly (range 1 11)))
;;37076114526