(ns day08
  (:require
   [clojure.core.matrix :as matrix]
   [aoc.core :refer [read-input-lines map-all]]
   [medley.core :as m]))

(defn to-grid
  "Parse a two-dimensional grid of single digit numbers into a map structure.
  The keys are the [x y] coordinates and the values are the value at that coord
  The top left is [0 0]. The bottom right is [col-n row-n].
  "
  [raw]
  (->>
   (map-indexed (fn [y line]
                  (map-indexed (fn [x char]
                                 [x y (parse-long (str char))]) line)) raw)
   (reduce (fn [g row]
             (reduce (fn [g [x y v]]
                       (assoc g [x y] v)) g row)) {})))
(def compass {:north [0 -1]
              :east [1 0]
              :south [0 1]
              :west [-1 0]})

(defn neighbors
  "Get all neigbors of pos in a direction"
  [g pos dir]
  (->> pos
       (iterate #(matrix/add % dir))
       rest
       (map g)
       (take-while some?)))

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
