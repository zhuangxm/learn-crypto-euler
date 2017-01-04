(ns euler.problem84)

(def tags [:go :a1 :cc1 :a2 :t1 :r1 :b1 :ch1 :b2 :b3 :jail
      :c1 :u1 :c2 :c3 :r2 :d1 :cc2 :d2 :d3 :fp :e1 :ch2
      :e2 :e3 :r3 :f1 :f2 :u2 :f3 :g2j :g1 :g2 :cc3 :g3 :r4
      :ch3 :h1 :t2 :h2])

(def pos-m (zipmap tags (range 40)))

(defn ->pos
  [newpos current]
  (pos-m newpos))

(defn move
  [n current]
 (mod (+ n current) 40) )

(defn next-pos
  [index]
  (mod (inc index) 40))

(defn back
  [n current]
  (mod (- current n -40) 40))

(defn sub-tag?
  [s w]
  (.startsWith (str s) (str w)))

(defn ->next-pos
  [tag current]
  (loop [p (next-pos current)]
    (if (sub-tag? (nth tags p) tag)
      p
      (recur (next-pos p)))))

(def ->go (partial ->pos :go))
(def ->jail (partial ->pos :jail))
(def ->c1 (partial ->pos :c1))
(def ->e3 (partial ->pos :e3))
(def ->h2 (partial ->pos :h2))
(def ->r1 (partial ->pos :r1))
(def ->next-r (partial ->next-pos :r))
(def ->next-u (partial ->next-pos :u))
(def ->back3 (partial back 3 ))

(def cc-cards (concat [->go ->jail] (repeat 14 identity)))

(def ch-cards [->go ->jail ->c1 ->e3 ->h2 ->r1
               ->next-r ->next-r ->next-u ->back3
               identity identity identity identity identity identity])

(defn move-first-to-back
  [coll]
  (concat (rest coll) [(first coll)]))

(defn default-action
  "handle :g2j :cc :ch"
  [current cc ch]
  (cond
    (= :g2j (nth tags current)) [(->jail current) cc ch]
    (sub-tag? (nth tags current) :cc) [((first cc) current)
                                       (move-first-to-back cc)
                                       ch]
    (sub-tag? (nth tags current) :ch) [((first ch) current)
                                       cc
                                       (move-first-to-back ch)]
    :else [current cc ch]))

(defn row-dice
  [dice-count dice-side]
  (->> #(rand-int dice-side)
    (repeatedly dice-count)
    (map inc)
    (apply +)))

(defn try-game
  [times dice-side]
  (prn " times: " times " dice-side: " dice-side)
  (loop [cc (shuffle cc-cards)
         ch (shuffle ch-cards)
         c times
         m {}
         current 0]
    #_(prn m " " current)
    (if (<= c 0)
      m
      (let [d (row-dice 2 dice-side)
            np (move d current)
            [new-c new-cc new-ch] (default-action np cc ch)
            new-tag (nth tags new-c)
            new-m (merge-with + m {new-tag 1})]
        (recur new-cc new-ch (dec c) new-m new-c)))))

(defn sort-result
  [m]
  (let [total (apply + (map second m))]
    (->> m
      (sort-by second)
      (map (fn [[k v]] [k v (/ v total 1.0)]))
      (reverse)
      (map first)
      (map pos-m)
      (take 3)
      (apply format "%02d%02d%02d"))))

(defn answer
  []
  (sort-result (try-game 1000000 4)))

#_(sort-result (try-game 1000000 6))