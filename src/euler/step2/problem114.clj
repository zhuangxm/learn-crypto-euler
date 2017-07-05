(ns euler.step2.problem114)

(declare answer-memorize)

(defn answer-black-red
  [n black red min-red]
  (cond
    (or (== (+ red black) n) (<= n min-red)) 1
    (< n (+ black red)) 0
    :default (answer-memorize (- n black red 1) min-red)))

(def answer-black-red-memoize answer-black-red)

(defn answer-red
  [n red min-red]
  (cond
    (or (zero? red) (= n  red)) 1
    (< n red) 0
    :default (->> (range 0 (inc (- n red)))
                  (map #(answer-black-red-memoize n % red min-red))
                  (reduce +))))

(def answer-red-memoize answer-red)

(defn answer
  [n min-red]
  (cond
    (< n min-red) 1
    :default  (->> (cons 0 (range min-red (inc n)))
                   (map #(answer-red-memoize n % min-red))
                   (reduce +))))

(def answer-memorize (memoize answer))

(comment
 (answer 50 3))

