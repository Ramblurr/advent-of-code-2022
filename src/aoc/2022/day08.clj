(ns aoc.2022.day08
  (:require
   [aoc.grid :as grid]
   [aoc.core :refer [read-input-lines map-all]]
   [medley.core :as m]))

(defn visible? [g coord]
  (->>  grid/cardinals
        (map #(grid/coords-in-dir g coord %))
        (map-all #(get g %))
        (map (fn [neighbors] (every? #(< % (get g coord)) neighbors)))
        (some true?)))

(defn part1 [fname]
  (let [g (grid/to-grid (read-input-lines fname))]
    (->> (keys g)
         (map #(visible? g %))
         (filter true?)
         (count))))

(defn scenic-score [g coord]
  (->> grid/cardinals
       (map #(grid/coords-in-dir g coord %))
       (map-all #(get g %))
       (map-all #(< % (get g coord)))
       (map #(m/take-upto false? %))
       (map count)
       (apply *)))

(defn part2 [fname]
  (let [g (grid/to-grid (read-input-lines fname))]
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
