(ns crypto.week6
  (:require [clojure.math.numeric-tower :as math]))

(def n 179769313486231590772930519078902473361797697894230657273430081157732675805505620686985379449212982959585501387537164015710139858647833778606925583497541085196591615128057575940752635007475935288710823649949940771895617054361149474865046711015101563940680527540071584560878577663743040086340742855278549092581)

(defn factor
  [n]
  (let [a (-> (math/exact-integer-sqrt n) first inc)
        x (first (math/exact-integer-sqrt (- (* a a) n)))]
    [(- a x) (+ a x)]))

;;answer 1
#_(first (factor n))
;;13407807929942597099574024998205846127479365820592393377723561443721764030073662768891111614362326998675040546094339320838419523375986027530441562135724301

(defn factor2
  [n]
  (let [a (-> (math/exact-integer-sqrt n) first)]
    (loop [i (int (Math/pow 2 20))]
       (when (>= i 0)
         (let [c (+ a i)
               [x1 x2] (math/exact-integer-sqrt (- (* c c) n))]
           (if (and (= 0 x2)
                    (= n (* (- c x1) (+ c x1))))
             (- c x1)
             (recur (dec i))))))))

(def n2 648455842808071669662824265346772278726343720706976263060439070378797308618081116462714015276061417569195587321840254520655424906719892428844841839353281972988531310511738648965962582821502504990264452100885281673303711142296421027840289307657458645233683357077834689715838646088239640236866252211790085787877)

;;answer2
#_(factor2 n2)
;;25464796146996183438008816563973942229341454268524157846328581927885777969985222835143851073249573454107384461557193173304497244814071505790566593206419759

(def n3 720062263747350425279564435525583738338084451473999841826653057981916355690188337790423408664187663938485175264994017897083524079135686877441155132015188279331812309091996246361896836573643119174094961348524639707885238799396839230364676670221627018353299443241192173812729276147530748597302192751375739387929)

(defn factor3
  [n]
  (let [[x1 x2] (factor (* 24 n))
        x1 (/ x1 2)
        x2 (/ x2 2)
        x1 (if (odd? x1) (/ x1 3) (/ x1 2))
        x2 (if (odd? x2) (/ x2 3) (/ x2 2))]
    (if (< x1 x2)
      x1
      x2)))

#_(prn (factor3 n3))
;;remember to remove last N
;;21909849592475533092273988531583955898982176093344929030099423584127212078126150044721102570957812665127475051465088833555993294644190955293613411658629209

;;use euclidean algorithm to calculate d
;;code copy from https://rosettacode.org/wiki/Modular_inverse#Clojure

(defn extended-gcd
  "The extended Euclidean algorithm--using Clojure code from RosettaCode for Extended Eucliean
  (see http://en.wikipedia.orwiki/Extended_Euclidean_algorithm)
  Returns a list containing the GCD and the Bézout coefficients
  corresponding to the inputs with the result: gcd followed by bezout coefficients "
  [a b]
  (cond (zero? a) [(math/abs b) 0 1]
        (zero? b) [(math/abs a) 1 0]
        :else (loop [s 0
                     s0 1
                     t 1
                     t0 0
                     r (math/abs b)
                     r0 (math/abs a)]
                (if (zero? r)
                  [r0 s0 t0]
                  (let [q (quot r0 r)]
                    (recur (- s0 (* q s)) s
                           (- t0 (* q t)) t
                           (- r0 (* q r)) r))))))

(defn mul-inv
  " Get inverse using extended gcd.  Extended GCD returns
    gcd followed by bezout coefficients. We want the 1st coefficients
   (i.e. second of extend-gcd result).  We compute mod base so result
    is between 0..(base-1) "
  [a b]
  (let [b (if (neg? b) (- b) b)
        a (if (neg? a) (- b (mod (- a) b)) a)
        egcd (extended-gcd a b)]
      (if (= (first egcd) 1)
        (mod (second egcd) b)
        (str "No inverse since gcd is: " (first egcd)))))

(def c 22096451867410381776306561134883418017410069787892831071731839143676135600120538004282329650473509424343946219751512256465839967942889460764542040581564748988013734864120452325229320176487916666402997509188729971690526083222067771600019329260870009579993724077458967773697817571267229951148662959627934791540)

(defn decrypt
  [c n e]
  (let [[p q] (factor n)
        fai (- n p q -1)
        d (mul-inv e fai)
        m (.toByteArray (.modPow (biginteger c) (biginteger d) (biginteger n)))
        om (drop-while #(not= % 0) m)]
    (String. (byte-array om))))


#_(decrypt c n 65537)
;;" Factoring lets us break RSA."
