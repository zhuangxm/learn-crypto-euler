(ns crypto.week2
  (:require [clojure.string :as s])
  (:import (javax.crypto Cipher KeyGenerator SecretKey)
           (javax.crypto.spec SecretKeySpec IvParameterSpec)
           (java.security SecureRandom)))

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


#_(defn get-cipher
  [mode seed]
  (let [key-spec (SecretKeySpec. (get-raw-key seed) "AES")
        cipher (Cipher/getInstance "AES")]
    (.init cipher mode key-spec)
    cipher))

(defn aes-key
  [bs]
  (SecretKeySpec. bs "AES"))

(defn aes-encrypt
  [key input]
  (let [cipher (Cipher/getInstance "AES")]
    (.init cipher Cipher/ENCRYPT_MODE key)
    (.doFinal cipher input)))

(defn aes-descrypt
  [key input]
  (let [cipher (Cipher/getInstance "AES/ECB/NoPadding")]
    (.init cipher Cipher/DECRYPT_MODE key)
    (.doFinal cipher input)))

(defn bytes->string
  [bs]
  (->> bs
    (map char)
    (s/join "")))

(defn remove-padding
  [bs]
  (let [l (last bs)
        n (int l)]
    (if (= (take-last n bs) (repeat n l))
      (drop-last n bs)
      bs)))

(defn decrypt-block
  [key ct iv]
  (let [dr (aes-descrypt key ct)
        r (xor dr iv)
        r (remove-padding r)]
    (bytes->string r)))

(defn decrypt-ctr-block
  [key ct iv]
  (let [dr (aes-encrypt key iv)
        dr (take (count ct) dr)]
    (bytes->string (xor dr ct))))


(defn aes-cbc-decrypt
  [key-str input-str]
  (let [key-bs (byte-array (->bytes key-str))
        key (aes-key key-bs)
        input-blocks (partition-all 32 input-str)
        _ (prn "input-blocks size: " (count input-blocks))
        input-bs (vec (map #(-> % ->bytes byte-array) input-blocks))
        sizes (map count input-bs)
        iv (first input-bs)
        cts (rest input-bs)]
    (s/join "" (map #(decrypt-block key
                         (nth input-bs %)
                         (nth input-bs (dec %)))
         (range 1 (count input-blocks)) ))))

(defn inc-bytes
  [str n]
  (->>
    (BigInteger. str 16)
    (+ n )
    biginteger
    (format "%032x")
    ->bytes
    byte-array))

(defn aes-ctr-decrypt
  [key-str input-str]
  (let [key-bs (byte-array (->bytes key-str))
        key (aes-key key-bs)
        input-blocks (partition-all 32 input-str)
        input-bs (vec (map #(-> % ->bytes byte-array) input-blocks))
        sizes (map count input-bs)
        iv (subs input-str 0 32)
        _ (prn "iv: " iv)]
    (s/join "" (map #(decrypt-ctr-block key
                                    (nth input-bs %)
                                    (inc-bytes iv (dec %)))
                    (range 1 (count input-blocks)) ))))


(defn java-aes-decrypt
  [key-str input-str]
  (let [key-bs (byte-array (->bytes key-str))
        key (aes-key key-bs)
        cipher (Cipher/getInstance "AES/CBC/PKCS5Padding")
        iv (->> input-str ->bytes (take 16) byte-array)]
    (.init cipher Cipher/DECRYPT_MODE key (IvParameterSpec. iv))
    (String. (.doFinal cipher (->> input-str (drop 32) ->bytes byte-array)))))

(prn (aes-cbc-decrypt "140b41b22a29beb4061bda66b6747e14"
                  "4ca00ff4c898d61e1edbf1800618fb2828a226d160dad07883d04e008a7897ee2e4b7465d5290d0c0e6c6822236e1daafb94ffe0c5da05d9476be028ad7c1d81"))

(prn (aes-cbc-decrypt "140b41b22a29beb4061bda66b6747e14"
                      "5b68629feb8606f9a6667670b75b38a5b4832d0f26e1ab7da33249de7d4afc48e713ac646ace36e872ad5fb8a512428a6e21364b0c374df45503473c5242a253"))

(prn (aes-ctr-decrypt "36f18357be4dbd77f050515c73fcf9f2"
                      "69dda8455c7dd4254bf353b773304eec0ec7702330098ce7f7520d1cbbb20fc388d1b0adb5054dbd7370849dbf0b88d393f252e764f1f5f7ad97ef79d59ce29f5f51eeca32eabedd9afa9329"))

(prn (aes-ctr-decrypt "36f18357be4dbd77f050515c73fcf9f2"
                      "770b80259ec33beb2561358a9f2dc617e46218c0a53cbeca695ae45faa8952aa0e311bde9d4e01726d3184c34451"))

(prn (java-aes-decrypt "140b41b22a29beb4061bda66b6747e14"
                       "5b68629feb8606f9a6667670b75b38a5b4832d0f26e1ab7da33249de7d4afc48e713ac646ace36e872ad5fb8a512428a6e21364b0c374df45503473c5242a253"))
