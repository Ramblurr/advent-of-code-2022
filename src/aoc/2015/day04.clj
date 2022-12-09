(ns aoc.2015.day04
  (:import [java.security MessageDigest])
  (:require
   [aoc.core :as core :refer [read-input read-input-lines]]
   [medley.core :as m]
   [clojure.string :as str]))

(defn md5 [^String s]
  (let [algorithm (MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (BigInteger. 1 raw))))

(defn solve [key n]
  (let [prefix (repeat n \0)]
    (m/find-first #(= prefix (take n (md5 (str key %)))) (range))))

(def input (read-input "2015/day04.txt"))

(defn part-1 [input]
  (solve (str/trim input) 5))

;; (part-1 input)
;; => 346386

(defn part-2 [input]
  (solve (str/trim input) 6))

;; (part-2 input)
;; => 9958218
