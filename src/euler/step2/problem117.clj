(ns euler.step2.problem117)

(declare answer-memorize)

(defn answer-black-red
  [n black red]
  (cond
    (or (== (+ red black) n) (<= n red)) 1
    (< n (+ black red)) 0
    :default (answer-memorize (- n black red))))

(def answer-black-red-memoize answer-black-red)

(defn answer-red
  [n red]
  (cond
    (or (zero? red) (= n  red)) 1
    (< n red) 0
    :default (->> (range 0 (inc (- n red)))
                  (map #(answer-black-red-memoize n % red))
                  (reduce +))))

(def answer-red-memoize answer-red)

(defn answer
  [n]
  (cond
    (< n 2) 1
    :default  (->> [0 2 3 4]
                   (map #(answer-red-memoize n %))
                   (reduce +))))

(def answer-memorize (memoize answer))

(comment
 (answer 50))

