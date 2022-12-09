(ns aoc.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-input
  [day]
  (slurp (io/resource day)))

(defn read-input-lines
  "Reads the days input and splits it into a vector by line"
  [day]
  (-> (read-input day)
      (str/split-lines)))

(defn split-by
  "Splits each item in coll by the regex pattern"
  [pattern coll]
  (map #(str/split % pattern) coll))

(defn map-all [f coll]
  (map (fn [v] (map f v)) coll))

(defn reduce-all [f val coll]
  (map (fn [v] (reduce f val v)) coll))

(defn pull-ints
  "Pull all the intgers out of the string
  From: https://github.com/mcpower/adventofcode/blob/52d4b64b777cc76883e4d987f798d80e37a792f4/utils.py#L56
  "
  [s]
  (map parse-long
       (re-seq #"(?:(?<!\d)-)?\d+" s)))

(defn transpose
  "Transpose a list of lists"
  [coll]
  (apply mapv vector coll))

(defn third
  "Get the third element in coll"
  [coll]
  (second
   (rest coll)))

(defn peek-n
  "Like clojure.core/peek but peeks the top n elements."
  [n coll]
  (into [] (take-last n coll)))

(defn pop-n
  "Like clojure.core/pop but pops the top n elements."
  [n coll]
  (into []
        (drop-last n coll)))

(defn to-indexed-map
  "Converts a list to a map where the key is the index and the value is the value in the list at that index.
  [:a :b :c] -> {0 :a 1 :b 2 :c}
  "
  [v]
  (reduce merge {}
          (map-indexed (fn [i v] {i v}) v)))

(defn concatv
  "Like clojure.core/concat but always returns a vector"
  [& xs]
  (into []
        (apply concat xs)))

(defn to-matrix
  "Read a grid of characters into a 2 dimensional matrix"
  [raw]
  (into []
        (for [line raw]
          (into []
                (for [char line]
                  (parse-long (str char)))))))

(defn count-lines
  "Given a string, count the number of lines."
  [str]
  (inc (count (re-find #"\n" str))))
