(ns aoc.grid
  (:require
   [aoc.core :as core]
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

;; (def updown->cardinal {"R" :east "L" :west "U" :north "D" :south})
(def updown->cardinal {\R :east \L :west \U :north \D :south})

(defn parse-move
  "Given a single-line string, transforms it from a U/D/R/L + number direction
  (with or without whitespace) into a tuple of [direction amount]
   example:
    'U1'  -> [:north 1]
    'S 5' -> [:south 5]
  "
  [move]
  (when move
    (assert (= 1 (core/count-lines move)) (format "A move must be only a single line. Bad move: %s" move))
    ((juxt #(get updown->cardinal (first %)) #(first (core/pull-ints %))) move)))

(defn step-moves
  "Given a sequence of move tuples (see parse-move) returns
  a sequence of cartesian delta coords.
  example:
     []                     -> ()
     [[:north 2]]           -> ([0 -1] [0 -1])
     [[:south 1] [:east 1]] -> ([0 1] [1 0])"
  [moves]
  (mapcat (fn [[dir magnitude]] (repeat magnitude (compass dir))) moves))

(defn walk
  "Given a starting coord and a sequence of cartesian delta coords (see step-moves)
  returns every coord reached along the way (in order)."
  [start delta-coords]
  (reductions (partial mapv +)
              start delta-coords))
