(ns euler.problem87
  (:require [euler.util :refer :all]))

;;analyze sqrt(50,000,000) = 7071.
;; (last prime-10000) 9973 , 9973 > 7071.
;; so the range of prime-10000 is enough

(defn combs-by-pa-pb
  [max-num ps pa pb]
  (let [init (+ (* pa pa) (* pb pb pb))]
    (->> ps
      (map (fn [pc] [pa pb pc (+ init (* pc pc pc pc))]))
      (take-while #(<= (last %) max-num)))))

(defn combs-by-pa
   [max-num ps pa]
  (->> ps
    (map #(combs-by-pa-pb max-num ps pa %))
    (take-while seq)
    (mapcat identity)))

(defn combs
  [max-num ps]
  (->> ps
    (map #(combs-by-pa max-num ps %))
    (take-while seq)
    (mapcat identity)
    (map last)
    (set) ;;filter duplicate number
    (count)))


#_(combs 50000000 prime-10000)

;; 1097343 time: 3,423ms