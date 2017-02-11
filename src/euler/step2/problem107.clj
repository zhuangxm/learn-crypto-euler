(ns euler.step2.problem107)

(def sample [[0 16 12 21 0 0 0]
             [16 0 0 17 20 0]
             [12 0 0 28 0 31 0]
             [21 17 28 0 18 19 23]
             [0 20 0 18 0 0 11]
             [0 0 31 19 0 0 27]
             [0 0 0 23 11 27 0]])

(defn parse-line
  [line]
  (->>
    (clojure.string/split line #",")
    (map #(if (= "-" %) 0 (Integer/parseInt %)))
    vec))

(defn distance
  [net node1 node2]
  (nth (nth net node1) node2))

(defn line->map
  [net n]
  (->> (nth net n)
    (map-indexed #(vector n %1 %2))
    (filter #(pos? (last %)))))

(defn net->map
  [net]
  (->> (range (count net))
    (map #(vector % (line->map net %)))
    (into {})))

(defn map->segments
  [net-map]
  (->>
    (vals net-map)
    (apply concat)
    (filter #(< (first %) (second %)))))

(defn segments->map
  [segments]
  (->> segments
    (mapcat (fn [[n1 n2 w]] [[n1 n2 w] [n2 n1 w]]))
    (group-by first)))

(defn connected-nodes
  [net-map nodes]
  (->> nodes
    (mapcat #(get net-map %))
    (map #(second %))
    (set)))

(defn connected?
  [n net-map]
  (let [all-nodes (set (range n))]
    (loop [to-be-nodes #{0} nodes #{0}]
      (if-not (seq to-be-nodes)
        (= nodes all-nodes)
        (let [new-nodes (connected-nodes net-map to-be-nodes)]
          (recur (clojure.set/difference new-nodes nodes)
            (clojure.set/union new-nodes nodes)))))))

(defn double-weight
  [net-map]
  (->> (vals net-map)
    (apply concat)
    (map last)
    (apply +)))

(defn load-network
  []
  (->> "resources/p107_network.txt"
    slurp
    (clojure.string/split-lines)
    (map parse-line)
    vec))

(defn cut-one-index
  [segments index]
  (->> segments
    (map-indexed #(when-not (== index %1)
                    %2))
    (filter identity)))

(defn cut-one
  [n segments]
  (let [segments (sort-by last > segments)]
    (->> (range (count segments))
      (map #(cut-one-index segments %))
      (map segments->map)
      (filter #(connected? n %))
      (first))))

(defn simple-one
  [net]
  (let [n (count net)
        m (net->map net)
        segments (map->segments m)]
    (loop [s segments]
      (prn "segments: " (count s))
      (let [next-one (cut-one n s)]
        (if next-one
          (recur (map->segments next-one))
          s)))))

(defn answer
  [net]
  (let [original-weight (double-weight (net->map net))
        saved-one (simple-one net)
        after-weight (double-weight (segments->map saved-one))]
    (prn original-weight after-weight)
    (/ (- original-weight after-weight)
       2)))

#_(answer (load-network))

;;259679
