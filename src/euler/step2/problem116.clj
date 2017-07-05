(ns euler.step2.problem116)

(declare answer-memorize)

(defn answer-black-red
  [n black red]
  (cond
    (or (== (+ red black) n) (<= n red)) 1
    (< n (+ black red)) 0
    :default (answer-memorize (- n black red) red)))

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
  [n red]
  (cond
    (< n red) 1
    :default  (->> [0 red]
                   (map #(answer-red-memoize n %))
                   (reduce +))))

(def answer-memorize (memoize answer))

(defn solutions
  [n]
  (+ (answer n 2) (answer n 3) (answer n 4) -3))

(comment
 (solutions 50))

