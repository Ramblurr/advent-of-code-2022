(ns aoc.2022.day15
  (:require
   [clojure.core.matrix :as matrix]
   [thi.ng.geom.rect :as gr]
   [thi.ng.geom.utils :as gu]
   [thi.ng.geom.core :as g]
   [thi.ng.geom.polygon :as gp]
   [aoc.grid :as grid]
   [aoc.core :as core :refer [read-input-lines]]))

(def sample (read-input-lines "2022/day15-sample.txt"))
(def input (read-input-lines "2022/day15.txt"))

(defn parse-pairs [input]
  (->> input
       (map core/pull-ints)
       (flatten)
       (partition 2)
       (partition 2)
       (core/map-all #(into [] %))))

(defn to-poly [[sensor-pos beacon-pos]]
  (gp/polygon2 (grid/manhattan-poly sensor-pos
                                    (grid/manhattan-dist sensor-pos beacon-pos))))

(defn enumerate-row [bounds y-row]
  (let [max-x (+ (-> bounds :p first) (-> bounds :size first))]
    (loop [leftmost [(first (:p bounds)) y-row]
           gathered []]
      (let [next-point (update leftmost 0 inc)]
        (if (> (first next-point) max-x)
          gathered
          (recur next-point (conj gathered leftmost)))))))

(defn polys-contain? [diamonds p]
  (if (some #(g/contains-point? % p) diamonds)
    p
    nil))

(defn part-1 [y-row input]
  (let [pairs (parse-pairs input)
        diamonds (pmap to-poly pairs)
        beacons (into #{} (map second pairs))
        n-beacons-at-row (count (filter #(= y-row (second %)) beacons))
        bounds (gu/coll-bounds diamonds)
        y-row-points (enumerate-row bounds y-row)]
    {:nbar n-beacons-at-row
     :npoints (count y-row-points)
     :result (->> y-row-points
                  (pmap #(polys-contain? diamonds %))
                  (filter some?)
                  count)}))

(defn borders
  "Return a sequence of len 4 where each item in the sequence
  is a list of points that are one unit beyond the perimeter of the diamonl"
  [diamond]
  (let [[top-right-edge
         bottom-right-edge
         bottom-left-edge
         top-left-edge] (g/edges diamond)]
    [(grid/enumerate-line (matrix/add (first top-right-edge) [0 -1])
                          (matrix/add (second top-right-edge) [1 0]))

     (grid/enumerate-line (matrix/add (second bottom-right-edge) [0 1])
                          (matrix/add (first bottom-right-edge) [1 0]))

     (grid/enumerate-line (matrix/add (second bottom-left-edge) [-1 0])
                          (matrix/add (first bottom-left-edge) [0 1]))

     (grid/enumerate-line (matrix/add (first top-left-edge) [-1 0])
                          (matrix/add (second top-left-edge) [0 -1]))]))

(defn tuning-freq [[x y]]
  (+ (int y) (* (int x) 4000000)))

(defn part-2 [input size]
  ;; the insight here is that our bingo point must lie somewhere adjacent
  ;; to a point on the perimeter one of the diamonds.
  ;; the point that lies inside the bb but is not inside any of the polys is our point
  ;; use pmap to speed of the search using multiple cores
  (let [bb (gr/rect 0 0 size)
        pairs (parse-pairs input)
        diamonds (pmap to-poly pairs)
        border-lines (pmap borders diamonds)
        combined (set (apply concat (apply concat border-lines)))
        bingo (filter some? (pmap #(when (and (g/contains-point? bb %) (not (polys-contain? diamonds %))) %) combined))]

    (map tuning-freq bingo)))

(comment
  (part-1 10 sample) ;; rcf
  ;; => {:nbar 1, :npoints 36, :result 26}

  (time (part-1 2000000 input))
  ;; => {:nbar 1, :npoints 6671216, :result 4665948}
  ;; "Elapsed time: 57187.848989 msecs"

  (time (part-2 input 4000000))
  ;; => (13543690671045)
  ;; "Elapsed time: 656616.908538 msecs"
  )
