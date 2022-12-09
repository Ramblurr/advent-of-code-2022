(ns aoc.2015.day03
  (:require
   [aoc.grid :as grid]
   [aoc.core :as core :refer [read-input read-input-lines]]
   [clojure.set :as set]))

(def input (read-input "2015/day03.txt"))

(def directions {\^ :north \v :south \> :east \< :west})

(defn part-1 [input]
  (->> input
       (map directions)
       (map (fn [dir] [dir 1]))
       (grid/step-moves)
       (grid/walk [0 0])
       (set)
       (count)))

(defn part-2 [input]
  (let [moves (->> input
                   (map directions)
                   (map (fn [dir] [dir 1])))
        ;; transpose + partition 2 gets us "every other"
        [santa robo] (core/transpose (partition 2 moves))]
    (count
     (set/union
      (->> santa
           (grid/step-moves)
           (grid/walk [0 0])
           (set))
      (->> robo
           (grid/step-moves)
           (grid/walk [0 0])
           (set))))))

(part-1 input)
;; => 2572

(part-2 input)
;; => 2631
