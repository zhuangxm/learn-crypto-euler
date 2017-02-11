(ns euler.step2.problem104)

(declare mem-fib)

(defn fib
  [n]
  (cond
    (== n 1) (bigint 1)
    (== n 2) (bigint 1)
    :else (+ (mem-fib (dec n))
             (mem-fib (- n 2)))))

(def mem-fib (memoize fib))

(def char9s (set (map char (range (int \1) (+ (int \1) 9)))))

(def ten-pow-10 (bigint (Math/pow 10 10)))

(defn last9s?
  [n]
  (->>  (rem n ten-pow-10)
    (str)
    (take-last 9)
    (set)
    (= char9s)))

(defn  first9s?
  [n]
  (->> (str n)
    (take 9)
    (set)
    (= char9s)))

(defn answer
  []
  (loop [n 3 fn-1 1 fn-2 (biginteger 1)]
    (when (< n 1000000)
      (when (zero? (rem n 10000))
          (prn n))
      (let [f (+ fn-1 fn-2)]
        (if (and (last9s? f)
                 (first9s? f))
          n
          (recur (inc n) f fn-1))))))

#_(answer)

;;329468