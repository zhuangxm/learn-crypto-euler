(ns euler.step2.problem128
  (:require [euler.util :as util]))

;;prime can only possible in 0-1 column.

(defn start-number
  [layer]
  (+ 2 (* 3 (dec layer) layer)))

(defn pd-count
  [ps number numbers]
  (->> numbers
       (map #(Math/abs (- % number)))
       (filter #(util/prime? % ps))
       count))

(defn zero-column 
  [layer & [ps]]
  (let [number (start-number layer)
        down (if (= number 2) 1 (start-number (dec layer)))
        up (start-number (inc layer))
        up-left (inc up)
        down-left (inc number)
        down-right (dec up)
        up-right (dec (start-number (+ 2 layer)))]
    [number (pd-count ps number [up-left down-right up-right])]))

(defn one-column 
  "layer >= 2"
  [layer & [ps]]
  (if (< layer 2)
    [0 0]
    (let [number (dec (start-number (inc layer)))
          up-left (start-number layer)
          down-left (start-number (dec layer))
          up-right (- (start-number (+ layer 2)) 2)]
        [number (pd-count ps number [up-left down-left up-right])])))  
         
(defn solutions 
  [prime-count max-count]
  (let [ps (util/lazy-primes)]
    (loop [s [1]
           layer 1]
      (let [a (zero-column layer ps)
            b (one-column layer ps)]
        (if (>= (count s) max-count)
          s
          (recur
            (cond-> s
              (= (last a) prime-count) (conj (first a))
              (= (last b) prime-count) (conj (first b)))
            (inc layer)))))))

(defn answer 
  [n]
  (->> (solutions 3 n)
       (take n)
       (last)))

(comment
  (time (answer 2000)))
