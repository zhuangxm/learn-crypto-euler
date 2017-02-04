(ns euler.problem96
  (:require [clojure.math.combinatorics :as combo]))

;;steps to solve a sudoku
;;1 find the cell of the only solutions
;;2 find two cells in a row col block that have two identical possibles
;;3 find three cells in a row col block that have three identical possibles
;;4 brute force

(defn nums->coll
  [str-nums]
  (vec (map #(Integer/parseInt (str %)) str-nums)))

(defn load-sudoku
  [file-name]
  (->>
    (clojure.string/split (slurp file-name) #"\n")
    (remove #(.startsWith % "Grid"))
    (map nums->coll)
    (partition 9)
    (vec)))

(def nums (set (range 1 10)))

(defn column-line
  [table col & [pred]]
  (->> table
    (map #(nth % col))
    (filter (or pred integer?))))

(defn row-line
  [table row & [pred]]
  (->> (nth table row)
    (filter (or pred integer?))))

(defn block
  [table row col & [pred]]
  (let [b-r (quot row 3)
        b-c (quot col 3)]
    (->> (range (* b-r 3) (* (inc b-r) 3))
      (map #(nth table %))
      (map #(subvec % (* b-c 3) (* (inc b-c) 3)))
      (apply concat)
      (filter (or pred integer?)))))

(defn num-in-position
  [table col row]
  (nth (nth table row) col))

(defn impossibles
  "deduce impossibles from other possibles"
  [other-possibles]
  (loop [im-ps #{} c (count other-possibles)]
    (if (>= c 2)
      (let [cs (combo/combinations other-possibles c)
            ps (->> cs
                 (map #(apply clojure.set/union %))
                 (filter #(= c (count %)))
                 (apply clojure.set/union))]
        (recur (clojure.set/union im-ps ps) (dec c)))
      im-ps)))

(defn possibles
  [table col row]
  (let [c (set (column-line table col))
        r (set (row-line table row))
        b (set (block table row col))
        pred #(and (set? %) (not= % (num-in-position table col row)))
        c-ps (impossibles (column-line table col pred))
        r-ps (impossibles (row-line table row pred))
        b-ps (impossibles (block table row col pred))
        im-ps (clojure.set/union c r b c-ps r-ps b-ps)
        ps (clojure.set/difference nums (disj im-ps 0))]
    (if (== 1 (count ps))
      (first ps)
      (when (seq ps) ps))))

(defn fill-row
  [table row row-index]
  (->> row
    (map-indexed
     (fn [index v]
       (let [v (num-in-position table index row-index)]
         (if (or (coll? v) (= 0 (num-in-position table index row-index)))
           (possibles table index row-index)
           v))))
    vec))

(defn fill-possibles
  [table]
  (->> table
    (map-indexed (fn [index v] (fill-row table v index)))
    vec))

(defn incorrect?
  [table]
  (some nil? (apply concat table)))

(defn simple-solve [table]
  (loop [t table]
    ;;(prn "reduce: " t)
    (let [after (fill-possibles t)]
      (when-not (incorrect? after)
        (if-not (= after t)
          (recur after)
          after)))))

(defn find-first-unsolved
  [table]
  (some identity
    (for [i (range 9) j (range 9) :let [p (num-in-position table i j)]]
      (when (coll? p)
        [i j p]))))

(defn try-table
  [table col row num]
  (assoc-in table [row col] num))

(defn brute-force
  [table]
  (let [[col row ps] (find-first-unsolved table)]
    (->> ps
      (map #(try-table table col row %))
      (map simple-solve)
      (filter identity)
      (map #(if-not (solve? %) (brute-force %) %))
      (some identity))))

(defn solve
  [table]
  (let [a (simple-solve table)]
    (if (solve? a)
      a
      (brute-force a))))

(def s-file "resources/p096_sudoku.txt")

#_(load-sudoku "resources/p096_sudoku.txt")

(defn solve?
  [table]
  (every? number? (apply concat table)))

(defn solve-all [file-name]
  (->> (load-sudoku file-name)
    (map solve)))

(defn answer [file-name]
  (->> (solve-all file-name)
    (map first)
    (map #(take 3 %))
    (map #(map * [100 10 1] %))
    (map #(apply + %))
    (apply +)))

#_(answer s-file)

;;24702
