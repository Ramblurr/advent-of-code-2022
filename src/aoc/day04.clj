(ns day04
  (:require [aoc.core :refer [read-input map-all reduce-all]]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn subsets? [[s1 s2]]
  (or
   (set/subset? s1 s2)
   (set/subset? s2 s1)))

(defn part1 [fname]
  (->>
   (-> (read-input fname)
       (str/split-lines))
   (map #(str/split % #","))
   (map-all #(update (mapv parse-long (str/split % #"-"))
                     1 inc))
   (map-all #(apply range %))
   (map-all #(into #{} %))
   (map subsets?)
   (map (fn [v] (if v 1 0)))
   (reduce +)))

(part1 "day04-sample.txt")
(part1 "day04.txt")

(defn overlaps? [[s1 s2]]
  (seq
   (set/intersection s1 s2)))

(defn part2 [fname]
  (->>
   (-> (read-input fname)
       (str/split-lines))
   (map #(str/split % #","))
   (map-all #(update (mapv parse-long (str/split % #"-"))
                     1 inc))
   (map-all #(apply range %))
   (map-all #(into #{} %))
   (map overlaps?)
   (map (fn [v] (if v 1 0)))
   (reduce +)))

(part2 "day04-sample.txt")
(part2 "day04.txt")
