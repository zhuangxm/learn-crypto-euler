(ns euler.step2.problem132
  (:require [euler.util :as util]))

(defn prime-factor? 
  [k prime]
  (== (.modPow BigInteger/TEN k
               (biginteger (* 9 prime)))
      1))
  
(defn answer 
  [k factors-num]
  (let [ten BigInteger/TEN
        k (.pow ten k)
        primes (util/lazy-primes)]
    (->> primes
         (filter #(prime-factor? k %))
         (take factors-num)
         (apply +))))

;;(answer 9 40)
;;843296

    
