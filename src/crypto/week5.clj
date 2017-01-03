(ns crypto.week5
  (:import [java.util HashMap]
           [java.math BigInteger]))

(def g (BigInteger. "11717829880366207009516117596335367088558084999998952205599979459063929499736583746670572176471460312928594829675428279466566527115212748467589894601965568"))
(def p (BigInteger. "13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084171"))
(def h (BigInteger. "3239475104050450443565264378728065788649097520952449527834792452971981976143292558073856937958553180532878928001494706097394108577585732452307673444020333"))

(def b (biginteger (Math/pow 2 20)))
(def g-b (.modPow g b p))

(defn cal-h-g-x1
  [^BigInteger p ^BigInteger h ^BigInteger g x1]
  (-> (.modPow g (biginteger x1) p)
    (.modInverse p)
    (.multiply h)
    (.mod p)))

(defn hash-t
  []
  (let [r (inc (int (Math/pow 2 20)))
        m (HashMap. r)]
    (dorun (for [a (range r)]
             (do
               (when (== 0 (mod a 10000)) (prn "x1: " a))
               (.put m (cal-h-g-x1 p h g a)
                     (biginteger a)))))
    m))

(defn lookup
  [m]
  (loop [x0 0]
    (let [g-b-x0 (.modPow g-b (biginteger x0) p)
          v (.get m g-b-x0)]
      (when (== 0 (mod x0 10000)) (prn "x0: " x0))
      (if v
        [(biginteger x0) v]
        (when (< x0 b)
          (recur (inc x0)))))))

(defn c-xo-x1
  [^BigInteger x0 ^BigInteger x1 ^BigInteger b]
  (-> x0
    (.multiply b)
    (.add x1)
    (.mod p)))

(defn solution
  []
  (let [m (hash-t)
        [x0 x1] (lookup m)
        _ (prn [x0 x1])]
    (c-xo-x1 x0 x1 b)))
