(ns euler.util)

(defn filter-multi-p
  [coll p]
  (doall (filter #(not= 0 (mod % p)) coll)))

(defn primes
 "all primes less than n"
  [n]
  (loop [col (range 3 n 2)
         ps [2]]
    (if-let [p (first col)]
      (recur (filter-multi-p col p)
        (conj ps (first col)))
      ps)))

(defonce prime-10000 (primes 10000))
(defonce prime-10000-set (set prime-10000))

(defn prime?
  [n]
  "check if n is prime? n < 1,000,000,000,000"
  (or (prime-10000-set n)
      (every? #(not= 0 (mod n %)) prime-10000)))

(defn nearest-sqrt
  [n]
  (int (Math/sqrt n)))

(defn can-sqrt?
  [n]
  (let [s (int (Math/sqrt n))]
    (== n (* s s))))

(defn divied-by?
  [n d]
  (== 0 (mod n d)))

