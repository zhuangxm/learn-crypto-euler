(ns euler.problem64)

(defn nearest-sqrt
  [n]
  (int (Math/sqrt n)))


(defn transform
   " (n + m) / d  transform to
    a + (n - b) / d , make  0 < (n - b) / d < 1,
    return a b "
  [n d m]
  (let [s (inc (nearest-sqrt n))
        a (inc (quot m d))
        b (- d (mod m d))]
      (loop [b1 b
            a1 a]
        (if (> (- s b1) d)
          (recur (+ b1 d) (inc a1))
          [a1 b1]))))

(defn reserve-divisor
  "handle a / (n - b) "
  [a n b]
  (let [d (/ (- n (* b b)) a)
        [r m] (transform n d b)]
    (cons [r [d n m]] (lazy-seq (reserve-divisor d n m)))))

(defn test-repeat
  [coll]
  (let [c (count coll)
        h (/ c 2)]
    (when (and (even? (count coll))
               (= (take h coll) (drop h coll)))
      (take h coll))))

(defn sqrt-seq
  [n]
  (let [s (nearest-sqrt n)]
    (reserve-divisor 1 n s)))

(defn solution
  [n]
  (reduce
   (fn [r c]
     (let [r (conj r c)]
       (if (test-repeat r)
        (reduced (test-repeat r))
        r )) ) [] (sqrt-seq n)))

(defn convergents
  [n]
  (->> (sqrt-seq n)
    (map first)
    (cons (nearest-sqrt n))
    (reductions (fn [[[h k] [h-1 k-1]] a]
                 [[(bigint (+ h-1 (* a h))) (bigint (+ k-1 (* a k)))] [h k]])
               [[1 0] [0 1]])
    (map first)
    (drop 1)))

(defn can-sqrt?
  [n]
  (let [s (int (Math/sqrt n))]
    (== n (* s s))))

(defn answer
  [n]
  (->> (range 2 (inc n))
    (filter (complement can-sqrt?))
    (map solution)
    (map count)
    (map odd?)
    (filter identity)
    (count)))