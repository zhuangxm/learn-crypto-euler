(ns euler.problem82
  (:require [clojure.set :refer :all]))

(defn seperate-numbers
  [s]
  (->> (clojure.string/split s #",")
    (map #(Integer/parseInt %))
    vec))

(defn load-matrix
  []
  (->> (clojure.string/split (slurp "resources/p082_matrix.txt") #"\n")
    (map seperate-numbers)
    vec))

(defn node
   "r c start from 0"
  [m [r c]]
  #_(prn "node m: " m " "  r " " c)
  (-> m
    (nth r)
    (nth c)))

(defn neighbors
  "not allow left"
  [l [r c]]
  (->> [[(dec r) c] [(inc r) c] #_[r (dec c)] [r (inc c)]]
    (filter (fn [[r1 c1]]
              (and (> l r1 -1)
                   (> l c1 -1))))
    set))

(defn open-neighbors
  [closed-sets neighbor-set]
  (difference neighbor-set closed-sets))

(defn select-current
  [open-set scores]
  #_(prn "in select-current")
  #_(prn "opne-set : " open-set)
  #_(prn "scores: " scores)
  (->> open-set
    (select-keys scores)
    (sort-by #(first (second %)))
    ffirst))

(defn update-score
   "score : [score from]"
  [score position p-score]
  (if (or (nil? score)
          (< p-score (first score)))
    [p-score position]
    score))

(defn update-scores
  [scores current m n-s]
  #_(prn "scores: " scores)
  (let [current-score (first (get scores current))]
    (->> n-s
      (reduce
       (fn [r v]
         (let [s (+ current-score (node m v))]
           (update r v #(update-score % current s))))
       scores))))

(defn construct-path
  [scores current]
  #_(prn "construct-path: " scores )
  (loop [path [[current (first (scores current))]]]
    (let [from (second (scores (ffirst path)))]
      (if-not from
        path
        (recur (cons [from (first (scores from))] path))))))

(defn wide-search
  "matrix like [[1 2 3] [4 5 6] [7 8 9]]"
  [matrix start]
  (prn "start: " start)
  (let [l (count matrix)]
    (loop [open-set #{start}
           close-set #{}
           scores {start [(node matrix start) nil]}]
      #_(prn "open-set: " open-set)
      #_(prn "close-set: " close-set)
      #_(prn "scrores: " scores)
      (let [current (select-current open-set scores)
            n-s (open-neighbors close-set (neighbors l current))]
        #_(prn "n-s: " n-s)
        (if (== (second current) (dec l))
          (construct-path scores current)
          (recur (union (difference open-set #{current}) n-s)
            (conj close-set current)
            (update-scores scores current matrix n-s)))))))

(def test-matrix [[131 673 234 103 18]
                  [201 96 342 965 150]
                  [630 803 746 422 111]
                  [537 699 497 121 956]
                  [805 732 524 37 331]])

(defn answer
 [m]
 (let [l (count m)]
   (->> (range l)
     (map #(vector % 0))
     (map #(wide-search m %))
     (map last)
     (map last)
     (apply min))))

#_(answer (load-matrix))

;;260324
