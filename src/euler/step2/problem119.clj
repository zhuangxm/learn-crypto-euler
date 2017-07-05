(ns euler.step2.problem119)

(defn sum-digits
  [n]
  (->> (str n)
       (map #(Long/parseLong (str %)))
       (reduce +)))

(defn powers
  [n max-number]
  (when-not (= 1 (sum-digits n))
    (->> (repeat n)
         (reductions *')
         (filter #(> % 10))
         (take-while #(> max-number %))
         (filter #(= (sum-digits %) n)))))

(defn answers
  [n-digit]
  (let [max-number (reduce *' (repeat n-digit 10))
        max-sum (*' 9 n-digit)]
    (->> (range 2 (inc max-sum))
         (mapcat #(powers % max-number))
         (sort))))

(comment
 (nth (take 30 (answers 100)) 29))
