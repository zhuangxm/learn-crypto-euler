(ns euler.step2.problem125
  (:require [euler.util :as util]))

; (a+ 0)^2 + (a + 1)^2 + ... (a + n)^2
;  == (n + 1) ( a^2 + a + n * (2n + 1) /6 )

(defn consecutive-power
  [a len]
  (let [n (dec len)]
    (* len
       (+ (* a a)
          (* n a)
          (/ (* n (inc (* 2 n)))
             6)))))

(defn palindromic?
  [n]
  (= (str n)
     (apply str (reverse (str n)))))

(defn solutions
  [a max-num]
  (->> (iterate inc 2)
       (map #(vector a % (consecutive-power a %)))
       (take-while #(< (last %) max-num))
       (filter #(palindromic? (last %)))))


(defn answer
  [max-num]
  (->> (range 1 (inc (util/nearest-sqrt max-num)))
       (mapcat #(solutions % max-num))
       (map last)
       (distinct) ;;remove duplicate.
       (reduce +)))

(comment
 (answer (reduce * (repeat 8 10))))