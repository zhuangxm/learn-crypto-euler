(ns euler.problem98
  (:require [clojure.math.combinatorics :as combo]))

(def words-file-name "resources/p098_words.txt")

(defn load-words-file [file-name]
  (clojure.string/split (slurp file-name) #","))

(defn simplify-key [[k v]]
  [(->> k (map last) sort) v])

(defn analyze-words
  [words]
  (->> words
    (map #(clojure.string/replace % #"\"" ""))
    (group-by frequencies)
    (filter #(<= 2 (count (last %))))
    (map simplify-key)
    (group-by first)
    (map (fn [[k v]] [k (map last v)]))
    (into {})))

(defn max-length
  [words-analyzed]
  (->> words-analyzed
    (map first)
    (map #(apply + %))
    (apply max)))

(defn powers
  [max-length]
  (let [max-num (Math/pow 10 max-length)]
    (->> (iterate inc 10)
      (map #(vector (* % %) %))
      (take-while #(< (first %) max-num))
      (group-by #(frequencies (str (first %))))
      (filter #(<= 2 (count (last %))))
      (map simplify-key)
      (group-by first)
      (map (fn [[k v]] [k (map last v)]))
      (map (fn [[k v]] [k (map #(map first %) v)]))
      (into {}))))

(defn match-word
  [num1 num2 word]
  (when-let [m (->> word
                 (map #(vector %1 %2) (str num1 num2))
                 (reduce (fn [r [k v]]
                           (if-let [old-v (get r k)]
                             (if (not= old-v v)
                               (reduced nil)
                               r)
                             (assoc r k v)))
                         {}))]
    (max num1 num2)))

(defn match-num-word
  [[num1 num2] [word1 word2]]
  (or (match-word num1 num2 (str word1 word2))
      (match-word num1 num2 (str word2 word1))))

(defn match-words
  [nums words]
  (when (seq nums)
    (let [nss (mapcat #(combo/combinations % 2) nums)
          wss (mapcat #(combo/combinations % 2) words)]
      (->> (for [n nss w wss]
              (match-num-word n w))
        (filter identity)))))

(defn find-match
  [powers words-combination]
  (->>
    (for [[k words] words-combination]
      (match-words (get powers k) words))))

(defn solve [file-name]
  (let [words (load-words-file file-name)
        ws (analyze-words words)
        len (max-length ws)
        ps (powers len)]
    (->> ws
      (find-match ps)
      (filter seq)
      (map #(apply max %))
      (apply max))))

#_(solve words-file-name)
