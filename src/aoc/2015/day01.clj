(ns aoc.2015.day01
  (:require
   [aoc.core :as core :refer [read-input read-input-lines]]
   [clojure.string :as str]))

(def sample "(())")
(def input (read-input "2015/day01.txt"))

(defn part-1 [input]
  (->>
   (str/split input #"")
   (map #(if (= % "(") 1 -1))
   (reduce +)))

(part-1 input)
;; => 232

(defn part-2 [input]
  (->>
   (str/split input #"")
   (map #(if (= % "(") 1 -1))
   (reductions +)
   (core/positions #{-1})
   first
   inc))

(part-2 input)
;; => 1783
