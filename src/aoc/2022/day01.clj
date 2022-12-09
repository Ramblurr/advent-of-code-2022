(ns aoc.2022.day01
  (:require [aoc.core :refer [read-input map-all reduce-all]]
            [medley.core :as m]
            [clojure.string :as str]))

(defn part-1 [fname]
  (let [in1 (->
             (read-input fname)
             (str/split #"\n\n"))]
    (->> in1
         (map str/split-lines)
         (map-all #(Integer/parseInt %))
         (reduce-all + 0)
         (apply max))))

(part-1 "2022/day01-sample.txt")

(part-1 "2022/day01.txt")

(defn part-2 [fname]
  (let [in1 (->
             (read-input fname)
             (str/split #"\n\n"))]
    (->> in1
         (map str/split-lines)
         (map-all #(Integer/parseInt %))
         (reduce-all + 0)
         (sort)
         (reverse)
         (take 3)
         (apply +))))

(part-2 "2022/day01-sample.txt")
(part-2 "2022/day01.txt")

