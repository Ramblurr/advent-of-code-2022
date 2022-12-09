(ns day08
  (:require
   [clojure.core.matrix :as matrix]
   [aoc.grid :refer [to-grid compass neighbors]]
   [aoc.core :refer [read-input-lines map-all]]
   [medley.core :as m]))

(defn visible? [grid coord]
  (->> (vals compass)
       (map #(neighbors grid coord %))
       (map (fn [neighbors] (every? #(< % (get grid coord)) neighbors)))
       (some true?)))

(defn part1 [fname]
  (let [g (to-grid (read-input-lines fname))]
    (->> (keys g)
         (map #(visible? g %))
         (filter true?)
         (count))))

(defn scenic-score [grid coord]
  (->> (vals compass)
       (map #(neighbors grid coord %))
       (map-all #(< % (get grid coord)))
       (map #(m/take-upto false? %))
       (map count)
       (apply *)))

(defn part2 [fname]
  (let [g (to-grid (read-input-lines fname))]
    (->> (keys g)
         (map #(scenic-score g %))
         (apply max))))

(part1 "day08-sample.txt")
;; => 21

(part1 "day08.txt")
;; => 1807

(part2 "day08-sample.txt")
;; => 8

(part2 "day08.txt")
;; => 480000
