(ns crypto.week4
  (:require [clojure.string :as s]
            [clj-http.client :as client]))

(defn ->bytes
  [str]
  (->> str
    (partition 2)
    (map s/join)
    (map #(Integer/parseInt % 16))))

(defn xor
  [b1 b2]
  (->>
    (map #(bit-xor %1 %2) b1 b2)))

(defn xor-str [str1 str2]
  (let [b1 (->bytes str1)
        b2 (->bytes str2)]
    (xor b1 b2)))

(defn ->hex-str
  [bs]
  (->> bs
    (map #(format "%02x" %))
    (apply str)))

(defn p1
  []
  (->hex-str
   (xor (->bytes "20814804c1767293b99f1d9cab3bc3e7")
        (xor (.getBytes "Pay Bob 100$")
             (.getBytes "Pay Bob 500$")))))

(def url "http://crypto-class.appspot.com/po?er=")
(def iv-and-c "f20bdba6ff29eed7b046d1df9fb7000058b1ffb4210a580f748b4ac714c001bd4a61044426fb515dad3f21f18aa577c0bdf302936266926ff37dbf7035d5eeb4")

(def iv (subs iv-and-c 0 32))
(def c (subs iv-and-c 32))
(def c1 (subs c 0 32))
(def c2 (subs c 32 64))
(def c3 (subs c 64 96))

(defn status
  [url c]
  (try
    (client/get (str url c))
    200
    (catch Exception e
      (:status (ex-data e)))))

(defn
  padding[n]
  (concat (repeat (- 16 n) 0) (repeat n n)))

(def paddings (map padding (range 1 17)))

(def all-zero (repeat 16 0))

(defn try-text
  [iv-bs guess-bs n]
  (-> iv-bs
    (xor (nth paddings n))
    (xor guess-bs)
    ->hex-str))

(defn guess
  [guess-bs n b]
  (concat (take (- 15 n) guess-bs) [b] (take-last n guess-bs)))

(defn try-one-byte
  [iv-bs c-str guess-bs n]
  (prn "try-one-byte: " guess-bs)
  (let [s (status url (str (try-text iv-bs guess-bs n) c-str))]
    (when (= s 404) guess-bs)))

;;why not lazy
(defn try-one
  [iv-str c-str guess-bs n]
  (->> (range 256)
    (some #(try-one-byte (->bytes iv-str) c-str (guess guess-bs n %) n))))

(defn decrypt
  [iv-str c-str]
  (reduce #(try-one iv-str c-str %1 %2) all-zero (range 0 16)))

(defn decrypt-last-block-n-padding
  [iv-str c-str n-padding]
  (reduce #(when %1 (try-one iv-str c-str %1 %2)) (nth paddings n-padding) (range (inc n-padding) 16)))

(defn decrypt-last-block
  [iv-str c-str]
  (->> (range 16)
    (some #(decrypt-last-block-n-padding iv-str c-str %))))

(defn remove-padding
  [bs]
  (let [l (last bs)
        n (int l)]
    (if (= (take-last n bs) (repeat n l))
      (drop-last n bs)
      bs)))


(String. (byte-array (remove-padding (concat '(84 104 101 32 77 97 103 105 99 32 87 111 114 100 115 32)
                             '(97 114 101 32 83 113 117 101 97 109 105 115 104 32 79 115)
                             '(115 105 102 114 97 103 101 9 9 9 9 9 9 9 9 9)))))

;; iv c1 (84 104 101 32 77 97 103 105 99 32 87 111 114 100 115 32)
;; c1 c2 (97 114 101 32 83 113 117 101 97 109 105 115 104 32 79 115)
;; c2 c3