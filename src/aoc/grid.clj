(ns aoc.grid
  (:require
   [clojure.core.matrix :as matrix]))

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

(def compass-diag
  {:northeast [1 -1]
   :northwest [-1 -1]
   :southeast [1 1]
   :southwest [-1 1]})

(defn adjacent-cardinal [pos]
  (->> (vals compass)
       (map #(matrix/add % pos))))

(defn adjacent-diagonal [pos]
  (->> (vals compass-diag)
       (map #(matrix/add % pos))))

(defn neighbors
  "Get all neigbors of pos in a direction"
  [g pos dir]
  (->> pos
       (iterate #(matrix/add % dir))
       rest
       (map g)
       (take-while some?)))
