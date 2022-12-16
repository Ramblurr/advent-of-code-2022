(ns aoc.grid
  (:require
   [aoc.core :as core]
   [clojure.core.matrix.linear :as matrix.linear]
   [clojure.core.matrix :as matrix]
   [clojure.string :as str]
   [medley.core :as m]))

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

(defn insert-with
  "Given a sequence of cartesian coordinates and a value, inserts all positions into the grid with that value"
  [g poss value]
  (m/deep-merge
   g
   (zipmap poss (take (count poss) (repeat value)))))

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

(defn adjacency-graph
  "Return an adjacency graph (as a map of node -> #{edges}) suitable for use with loom.

    adjacency-fn - the adjacency function (e.g., grid/adjacent-cardinal) that returns adjacent grid cells
    has-edge?    - a function (graph, from, to) -> bool that indicates if the node from has a directed edge to to.
    g            - the grid map

  "
  [adjacency-fn has-edge? g]
  (m/map-kv-vals (fn [pos _]
                   (set
                    (filter some?
                            (map (fn [neighbor]
                                   (when (has-edge? g pos neighbor)
                                     neighbor))
                                 (adjacency-fn pos)))))
                 g))

(defn slope [[x1 y1] [x2 y2]]
  (assert (not= x1 x2) "Cannot calculate slope of vertical lines")
  (quot
   (- y2 y1)
   (- x2 x1)))

(defn enumerate-line
  "Given two cartesian coordinates, return a list of all the points along the line"
  [[x1 y1] [x2 y2]]
  (cond
    (= x1 x2) ;; vertical line
    (for [y (range (min  y1 y2) (inc (max y1 y2)))]
      [x1 y])
    (= y1 y2) ;; horizontal line
    (for [x (range (min x1 x2) (inc (max x1 x2)))]
      [x y1])
    :else
    (let [slope (slope [x1 y1] [x2 y2])
          b (- y1 (* slope x1))]
      (for [x (range x1 (inc x2))]
        [x (+ (* slope x) b)]))))

(defn is-point? [p]
  (and (vector? p)
       (= 2 (count p))))

(defn points [g]
  (->> (keys g)
       (filter is-point?)))

(defn bounds
  "Calculate the bounds of the grid. Returns [p1 p2], where p1 is the minimum coord and p2 is the maximum coord"
  [g]
  [[(->> (points g) (map first) (apply min))
    (->> (points g) (map second) (apply min))]
   [(->> (points g) (map first) (apply max))
    (->> (points g) (map second) (apply max))]])

(defn render-grid
  "Returns an ascii representation of the grid.
     g - the grid map
     cell-fn - a function to render the given grid cell with form (grid, pos, value) -> str
  "
  [g cell-fn]
  (let [[[x-top y-top] [x-bottom y-bottom]] (bounds g)]
    (->>
     (for [y (range y-top (inc y-bottom))]
       (for [x (range x-top (inc x-bottom))]
         (cell-fn g [x y] (get g [x y]))))
     (map #(str/join "" %)))))

(defn manhattan-dist
  "Returns the manhattan distance between two cartesian coords."
  [a b]
  (-> (matrix/sub a b)
      (matrix.linear/norm 1)
      int))

(defn enumerate-distance
  "Given a cartesian coordinate and a maximum manhattan distance, returns a list of all the coords that are at most d distance away."
  [[origin-x origin-y] d]
  (->>
   (for [x (range (- d) (inc d))]
     (for [y (range (- d) (inc d))]
       (when (<=  (+ (abs x) (abs y)) d)
         [(+ origin-x x) (+ origin-y y)])))
   (apply concat)
   (remove nil?)))

(defn manhattan-poly
  "Given a manhattan distance and point return the corner points at the permimeter of the poly [right left top bottom]"
  [[origin-x origin-y] distance]
  (let [top [origin-x (- origin-y distance)]
        right [(+ origin-x distance) origin-y]
        bottom [origin-x (+ origin-y distance)]
        left [(- origin-x distance) origin-y]]
    [top right bottom left]))
