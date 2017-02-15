(ns euler.step2.problem109)

(def scores (conj (vec (range 1 21)) 25))

(def double-scores (map #(* 2 %) scores))

(def triple-scores (map #(* 3 %) (drop-last scores)))

(def order-of-dart {:single 0 :double 1 :triple 2})

(def all-scores
  (concat
   (map #(vector :single %) scores)
   (map #(vector :double %) double-scores)
   (map #(vector :triple %) triple-scores)))

(defn combine-sub-dart
  [s subs]
  (if-not (empty? subs)
    (map #(cons s %) subs)
    [[s]]))

(defn dart [score max-try]
  (when (and (> max-try 0)
             (> score 0))
      (->> all-scores
        (filter #(<= (last %) score))
        (mapcat #(combine-sub-dart % (dart (- score (last %))
                                           (dec max-try)))))))

(defn total-score
  [darts]
  (->> darts
    (map last)
    (apply +)))

(defn distinct-dart
  [score]
  (->> (dart score 2)
    (filter #(= (total-score %) score))
    (map sort)
    set))

(def mem-distinct-dart (memoize distinct-dart))

(defn double-ended-dart [score]
  (->> all-scores
    (filter #(and (<= (last %) score)
                  (= (first %) :double)))
    (mapcat #(combine-sub-dart %
                               (mem-distinct-dart (- score (last %)))))
    (filter #(= (total-score %) score))
    count))
(defn answer
  [n]
  (->> (range 2 n)
    (map double-ended-dart)
    (apply +)))


;;(answer 100)
;;31812
