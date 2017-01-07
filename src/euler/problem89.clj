(ns euler.problem89)

(defn simeplify-rule
  [one five ten]
  [[(apply str (repeat 9 one)) (str one ten)]
   [(str five one one one one) (str one ten)]
   [(apply str (repeat 8 one)) (str five one one one)]
   [(apply str (repeat 7 one)) (str five one one)]
   [(apply str (repeat 6 one)) (str five one)]
   [(apply str (repeat 5 one)) (str five)]
   [(apply str (repeat 4 one)) (str one five)]])

;;the last digit rule
(def one-rule (simeplify-rule "I" "V" "X"))
;;the last second rule
(def two-rule (simeplify-rule "X" "L" "C"))
;; the last third rule
(def three-rule (simeplify-rule "C" "D" "M"))


(def replace-rule (concat one-rule two-rule three-rule))
(def replace-m (into {} replace-rule))

(def match (re-pattern
            (->> replace-rule
              (map first)
              (clojure.string/join "|"))))

(defn simplify-roman
  [roman-str]
  (clojure.string/replace roman-str match replace-m))

(defn answer
  [file-name]
  (->>
    (clojure.string/split (slurp file-name) #"\n")
    (map (fn [str] (- (.length str) (.length (simplify-roman str)))))
    (reduce +)))

#_(answer "resources/p089_roman.txt")

;; 743