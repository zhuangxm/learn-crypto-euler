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
                   (> l c1 0))))
    set))

(defn neighbors-4-directions
  "not allow left"
  [l [r c]]
  (->> [[(dec r) c] [(inc r) c] [r (dec c)] [r (inc c)]]
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
  #_(prn "current: " current)
  (loop [path [[current (first (scores current))]]]
    (let [from (second (scores (ffirst path)))]
      #_(prn "path: " path "from: " from)
      (if-not from
        path
        (recur (cons [from (first (scores from))] path))))))

(defn target-last-column
  [[r c] target-column]
  (== c target-column))

(defn target-bottom-right
   [[r c] target-column target-row]
  (and (== c target-column)
       (== r target-row)))

(defn wide-search
  "matrix like [[1 2 3] [4 5 6] [7 8 9]]
  options support {:fn-neighbors .. :fn-target ..}"
  [matrix starts & [options]]
  (prn "start: " starts)
  (let [l (count matrix)
        fn-neighbors (get options :fn-neighbors neighbors)
        fn-targe (get options :fn-target #(target-last-column % (dec l)))]
    (loop [open-set (set (or (seq starts) #{[0 0]}))
           close-set #{}
           scores  (into {} (map #(vector % [(node matrix %) nil]) starts))]
      #_(prn "open-set: " open-set)
      #_(prn "close-set: " close-set)
      #_(prn "scrores: " scores)
      (let [current (select-current open-set scores)
            n-s (open-neighbors close-set (fn-neighbors l current))]
        #_(prn "n-s: " n-s)
        (if (fn-targe current)
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
     (wide-search m)
     last
     last)))

(defn answer-83
  [m]
  (let [l (count m)]
    (->>
      (wide-search m #{[0 0]}
                   {:fn-target #(target-bottom-right % (dec l) (dec l))
                    :fn-neighbors neighbors-4-directions})
      last
      last)))

#_(answer (load-matrix))

;;the answer is 260324
;;no optimization time: 176,647ms
;;first column only go don't right 175,433 ms
;;put all first column into start point. 2,486 ms

#_(answer-83 (load-matrix))

;;425185