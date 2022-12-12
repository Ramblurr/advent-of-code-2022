(ns aoc.grid
  (:require
   [aoc.core :as core]
   [clojure.core.matrix :as matrix]))

(defn to-grid
  "Parse a two-dimensional grid of characters into a map structure.
  The keys are the [x y] coordinates and the values are the value at that coord
  The top left is [0 0]. The bottom right is [col-n row-n].
  parse-cell-fn is a function that is given the value of a cell and returns the value.
  Input is a sequence of lines in the grid.
  See AOC 2022 Day 08 input as an example.
  "
  [raw parse-cell-fn]
  (->>
   (map-indexed (fn [y line]
                  (map-indexed (fn [x char]
                                 [x y (parse-cell-fn (str char))]) line)) raw)
   (reduce (fn [g row]
             (reduce (fn [g [x y v]]
                       (assoc g [x y] v)) g row)) {})))

(def cardinals
  "The four cardinal directions"
  [:north :east :south :west])
(def cardinal->delta
  "Given an cardinal direction, returns the cartesian coord delta to take one unit step in that direction"
  {:north [0 -1]
   :east  [1 0]
   :south [0 1]
   :west  [-1 0]})

(def ordinals
  "The four ordinal directions"
  [:northeast :northwest :southeast :southwest])

(def ordinal->delta
  "Given an ordinal direction, returns the cartesian coord delta to take one unit step in that direction"
  {:northeast [1 -1]
   :northwest [-1 -1]
   :southeast [1 1]
   :southwest [-1 1]})

(def compass->delta
  "Given a cardinal or ordinal direction, returns the cartesian coord delta to take one unit step in that direction"
  (merge cardinal->delta ordinal->delta))

(defn adjacent-cardinal
  "Returns the coordinates that are :east, :west, :north, and :south
  of the given cartesian coord."
  [pos]
  (->> (vals cardinal->delta)
       (map #(matrix/add % pos))))

(defn adjacent-cardinal-self
  "Returns the 5 coords that are cardinally adjacent to
  the given cartesian coord, including the point itself."
  [pos]
  (conj (adjacent-cardinal pos) pos))

(defn adjacent-ordinal
  "Returns the coordinates that are :north[east|west] and :south[east|west]
  of the given cartesian coord."
  [pos]
  (->> (vals ordinal->delta)
       (map #(matrix/add % pos))))

(defn adjacent-compass
  "Returns the coords that are cardinally and ordinally adjacent to
  the given cartesian coord."
  [pos]
  (concat (adjacent-ordinal pos) (adjacent-cardinal pos)))

(defn adjacent-compass-self
  "Returns the 9 coords that are cardinally and ordinally adjacent to
  the given cartesian coord, including the point itself."
  [pos]
  (conj (adjacent-compass pos) pos))

(defn adjacent?
  "Returns true if coord is adjacent to the reference coordinate. The definition of adjacent
  is given by the adjaceny type, one of:
          :cardinal      - see adjacent-cardinal
          :ordinal       - see adjacent-ordinal
          :compass       - see adjacent-compass
          :compass-self  - see adjacent-compass-self
          :cardinal-self - see adjacent-cardinal-self"
  [adjacency-type reference-coord coord]
  (let [adjacency-set
        (case adjacency-type
          :cardinal      (adjacent-cardinal reference-coord)
          :ordinal       (adjacent-ordinal     reference-coord)
          :compass       (adjacent-compass reference-coord)
          :compass-self  (adjacent-compass-self reference-coord)
          :cardinal-self (adjacent-cardinal-self reference-coord))]
    (some #(= coord %) adjacency-set)))

(defn coords-in-dir
  "Standing on coord pos and looking in direction dir,
  returns all other grid points in that direction until the end of the grid.
  Example:
    (coords-in-dir {[0 0] :a [0 1] :b [0 2] :c} [0 0] :south)
     => ([0 1] [0 2])

  "
  [g pos dir]
  (->> pos
       (iterate #(matrix/add % (compass->delta dir)))
       rest
       (take-while #(contains? g %))))

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
  (mapcat (fn [[dir magnitude]] (repeat magnitude (cardinal->delta dir))) moves))

(defn walk
  "Given a starting coord and a sequence of cartesian delta coords (see step-moves)
  returns every coord reached along the way (in order)."
  [start delta-coords]
  (reductions (partial mapv +)
              start delta-coords))
