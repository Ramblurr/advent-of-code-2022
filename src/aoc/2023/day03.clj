(ns aoc.2023.day03
  (:refer-clojure :exclude [parse-long symbol?])
  (:require [aoc.core :refer [read-input-lines  parse-long]]
            [aoc.grid :as grid]))

(def digits #{"0" "1" "2" "3" "4" "5" "6" "7" "8" "9"})

(defn read-grid [fname] (grid/to-grid (read-input-lines fname)
                                      (fn [v]
                                        (cond
                                          (= v ".") {}
                                          (digits v) {:type :digit :digit (parse-long v)}
                                          :else {:type :symbol :symbol v}))))

(defn digit? [v] (= :digit (:type v)))
(defn symbol? [v] (= :symbol (:type v)))

(defn walk-to-not-digit
  "Start at coord [x y] in the grid g, walk horizontally in a direction, dir (- or +) until we get to a
   grid cell that does not contain a digit, returns the x value of that cell. n is the upper/lower bound that we will walk"
  [g [x y] dir n]
  (or
   (->> (drop 1 (range 100))
        (map #(dir x %))
        (drop-while #(digit? (g [% y])))
        first)
   n))

(defn digit->part
  "Start at coord [x y] in grid g with n-cols columns. Walk horizontally in both directions accumulating digits.
  Returns a vector of [coord part-number] where coord is the coordinate of the first digit in the part and part-number is the full part-number"
  [g n-cols [x y]]
  (let [start-x (walk-to-not-digit g [x y] - 0)
        last-x (walk-to-not-digit g [x y] + n-cols)]
    [[(inc start-x) y]
     (->> (for [c (range start-x last-x)] (g [c y]))
          (map :digit)
          (map str)
          (apply str)
          (parse-long))]))

(defn adjacent-digits
  "Using compass directions, returns a list of the neighboring cells of ref-coord, that have digits in them."
  [g ref-coord]
  (for [coord (grid/adjacent-compass ref-coord)
        :when (digit? (g coord))] coord))

(defn grid->schematic
  "Convert the grid into a schematic. A schematic is a list of maps. Each map
  represents a symbol in the grid. The map contains the symbol and a a list of
  part-numbers adjacent to the symbol"
  [g]
  (let [[_ n-cols] (grid/grid-size g)]
    (->> g
         (filter #(symbol? (val %)))
         (map (fn [[coord v]]
                {:symbol (:symbol v)
                 :part-numbers (->> (adjacent-digits g coord)
                                    (map #(digit->part g n-cols %))
                                    (into {}))}))

         (into []))))

(defn part-1 [fname]
  (->> (read-grid fname)
       (grid->schematic)
       (map :part-numbers)
       (into {})
       vals
       (reduce +)))

(defn part-2 [fname]
  (->>
   (read-grid fname)
   (grid->schematic)
   (filter #(= (:symbol %) "*"))
   (filter #(= (count (:part-numbers %)) 2))
   (map :part-numbers)
   (map #(apply * (vals %)))
   (reduce +)))

(comment

  (part-1 "2023/day03-sample.txt")
  ;; => 4361
  (part-1 "2023/day03.txt")
  ;; => 512794

  (part-2 "2023/day03-sample.txt")
  ;; => 467835

  (part-2 "2023/day03.txt")
  ;; => 67779080
  )
