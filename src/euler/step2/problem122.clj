(ns euler.step2.problem122)

;;using find path algorithm like star*
(defn get-longest-collections
  [all]
  (let [longest (:longest all)]
    (->> (vals (dissoc all :longest))
         (mapcat identity)
         (filter #(= longest (count %))))))

(defn expand-collection
  [coll]
  (let [l (last coll)]
    (->> coll
         (map #(conj coll (+ l %))))))

(defn expand-one
  [all coll]
  ;;(prn ["expand" all coll])
  (reduce (fn [r v]
            (let [old-count (count (first (get r (last v))))
                  new-count (count v)]
              (cond
                (or (= old-count 0)
                    (< new-count old-count)) (assoc r (last v) [v])
                (= new-count old-count) (update r (last v) conj v)
                :default r)))
          all coll))

(defn expand
  [all]
  (let [colls (map expand-collection (get-longest-collections all))]
    (assoc (reduce expand-one all colls)
           :longest (count (ffirst colls)))))

(defn solutions
  [max-n]
  (let [s (range 1 (inc max-n))]
    (loop [all {:longest 1 1 [[1]]}]
      (if (every? #(get all %) s)
        all
        (recur (expand all))))))

(defn answer
  [max-n]
  (let [s (solutions max-n)]
    (->> (range 1 (inc max-n))
         (map #(get s %))
         (map #(count (first %)))
         (map dec)
         (reduce +))))

(comment
 (answer 200))
