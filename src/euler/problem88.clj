(ns euler.problem88
  (:require [euler.util :refer :all]))

;;analyze
;; n = ak + a(k-1) + a(k-2) + ... + a2 + a1 = ak * ... * a1
;; we can sort ak .. a1 to make ak >= a(k-1) >=  a(k-2) >= ..a2 >=a1
;; ak = (a(k-1) + a(k-2) + ... + a1) / (a(k-1) * a(k-2) * .. * a2  * a1 -1)
;; the change to ak biggest is to make (a(k-1) * a(k-2) * .. a2 .. * a1) == 2)
;; so a(k-1) ==2 and others 1 and the result ak = k
;; so there is at least one solution for each k. (k,2,..1),
;; for k =2 (2,2), others (3,2,1) (4,2,1,1) (5,2,1,1,1)
;; this is the biggest ak we find. and the ak + ... + a1 = 2k
;; next we try to find the least ak + ... + a1
;; the next thing we try to do is to make (ak-1 * ak-2 *... * a1) bigger but
;; still less than (ak-1 + ak-2 + .. + a1),
;;  try find combineation ak-1, ak-2 like (2 2) (3 1) (k, 1), ak-1 <= k.
;; make sure (ak-1 + ak-2 + .. + a1) < 2k and ak-1 * ak-2 + .. + a1 < 2k

(defn default-solution
  "return default solution
   [sum k 2 1 ... 1] "
  [k]
  (when (>= k 2)
    (concat [(* 2 k) k 2] (repeat (- k 2) 1))))

(defn other-solutions
  "as [ak-1 ak-2 .. a1], may only some head parts."
  [k as]
  #_(prn k " " as)
  (let [as (if (== 1 (last as))
             (concat as (repeat (- (dec k) (count as)) 1))
             as)
        sum (apply + (- (dec k) (count as)) as)
        p (apply * as)]
    (if (== (dec k) (count as))
      (when (and (> p 0) (divided-by? sum (dec p)))
        (let [ak (/ sum (dec p))]
          (when (< ak k)
            (concat [(+ sum ak)  ak] as))))
      (when (and (< (+ (first as) sum) (* 2 k)) (< (* (first as) p) (* 2 k)))
        (->> (iterate inc 1)
          (take (last as))
          (take-while #(< (apply * % (first as) as) (* 2 k)))
          (mapcat #(other-solutions k (conj as %))))))))

(defn solution
  [k]
  (when (>= k 2)
    (or
     (->> (iterate inc 2)
       (take (int (Math/sqrt (* 2 k))))
       (map #(other-solutions k [%]))
       (filter seq)
       (sort-by first)
       first)
     (default-solution k))))

;;this solution is too slow, should not be used in bigger number
(defn answer-slow
  [to]
  (->>
    (iterate inc 2)
    (take (dec to))
    (map solution)
    (map first)
    (set)
    (reduce + 0)))

;;---------------------------------------------------------------

;;another solution
;;becasuse any soltion can be expressed like:
;; for any number ak + ak-1 + ... + ak-n + (ak * ak-1 * ... * ak-n - (ak + ak-1 + ... _ ak-n) * 1)
;; like 5 * 4 * 3 = 60 , 5 + 4 + 3 = 12 so 5 , 4, 3 (60-12=48)* 1 is a solution of 51 number
(defn solutions
  [max-k as]
  (let [sum (apply + as)
        p (apply * as)
        c (count as)
        k (+ c p (- sum))]
    (when (and (<= k max-k) (>= p sum))
      (let [subs (->>
                   (range 2 (inc (last as)))
                   (take-while #(<= (* p %) (* 2 max-k)))
                   (map #(solutions max-k (conj as %)))
                   (apply merge-with min))]
        (merge-with min (when (> k 1) {k p}) subs)))))

(defn answers
  [max-k]
  (->>
    (range 2 (inc max-k))
    (map #(solutions max-k [%]))
    (apply merge-with min)
    (vals)
    (set)
    (reduce +)))

#_(answer 12000)

;;7587457 1,558ms
