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


;; https://web.archive.org/web/20150710134640/http://diditwith.net/2009/01/20/YAPESProblemSevenPart2.aspx
(defn lazy-primes
  "Generates an infinite, lazy sequence of prime numbers"
  []
  (letfn [(reinsert [table x prime]
                    (update-in table [(+ prime x)] conj prime))
          (primes-step [table d]
                       (if-let [factors (get table d)]
                         (recur (reduce #(reinsert %1 d %2) (dissoc table d) factors)
                           (inc d))
                         (lazy-seq (cons d (primes-step (assoc table (* d d) (list d))
                                                        (inc d))))))]
    (primes-step {} 2)))

(defonce prime-10000 (primes 10000))
(defonce prime-10000-set (set prime-10000))

(defn nearest-sqrt
  [n]
  (int (Math/sqrt n)))

(defn can-sqrt?
  [n]
  (let [s (int (Math/sqrt n))]
    (== n (* s s))))

(defn divided-by?
  [n d]
  (== 0 (mod n d)))

(defn prime?
  [n & [ps]]
  "check if n is prime?"
  (let [n-sqrt (inc (nearest-sqrt n))]
     (->> (or ps (lazy-primes))
          (take-while #(< % n-sqrt))
          (every? #(not= 0 (mod n %))))))

