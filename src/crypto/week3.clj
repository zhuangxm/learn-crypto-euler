(ns crypto.week3
  (:require [clojure.java.io :refer [file]])
  (:import [java.io RandomAccessFile]
           [java.security MessageDigest]))

(defn file-length
  [file-name]
  (.length (file file-name)))

(defn open
  [file-name]
  (RandomAccessFile. file-name "r"))

(defn close
  [raf]
  (.close raf))

(defn get-block
  [raf file-length buf block-size block-num]
  (.seek raf (* block-num block-size))
  (let [l (if (> (* (inc block-num) block-size) file-length)
            (- file-length (* block-num block-size))
            block-size)]
    (.read raf buf 0 l)
    l))

(def md (MessageDigest/getInstance "SHA-256"));

(defn sha
  [buf len d]
  (.update md buf 0 len)
  (.digest md d 0 32)
  (System/arraycopy d 0 buf 1024 32))

(defn ->hex-str
  [bs]
  (->> bs
    (map #(format "%02x" %))
    (apply str)))

(defn calculte-hash
  [file-name]
  (let [raf (open file-name)
        fl (file-length file-name)
        block-size 1024
        block-count (inc (quot (dec fl) block-size))
        _ (prn "block count: " block-count)
        b (byte-array (+ 1024 32))
        d (byte-array 32)]
    (do
      (loop [bc block-count]
        (when (= 0 (mod bc 500))
                 (prn "block-count: " bc))
        (let [l (get-block raf fl b block-size (dec bc))
              _ (when (not= l 1024) (prn "block size: " l))
              _ (if (= bc block-count)
                  (sha b l d)
                  (sha b (+ l 32) d))]
          (when (> bc 1)
            (recur (dec bc)))))
      (close raf)
      (->hex-str (drop 1024 b)))))
