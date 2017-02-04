(ns euler.problem100
  (:require [euler.problem64 :refer (convergents)]))
;; 2 * a  * (a -1) = n * (n - 1)
;; 2 * ((2a - 1)^ 2 - 1) = (2n - 1)^2 -1
;; x = 2a - 1 , y = 2n -1
;; 2 (x^2 - 1) = y^2 - 1
;; y^2 - 2x^2 = -1
;; because  n > 10^12, so y > 2* 10^12 - 1

;;using convergents mathod to solve this problems

(defn solution
  [begin-n]
  (->> (convergents 2)
    (some (fn [[h k]]
            (when (and (>= h (dec (* 2 begin-n)))
                       (== (- (* h h) (* 2 k k)) -1))
              [h k])))
    (map #(/ (inc %) 2))
    (apply min)))

#_(solution (bigint (Math/pow 10 12)))
;;756872327473






