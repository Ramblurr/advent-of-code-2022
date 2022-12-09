(ns aoc.2022.day09
  (:require
   [aoc.core :refer [read-input-lines]]
   [aoc.grid :as grid]
   [clojure.core.matrix :as matrix]
   [clojure.math :as math]))

(defn visit [g kw pos]
  (-> g
      (assoc-in [pos kw] true)
      (assoc kw pos)))

(defn step-towards [g head tail]
  (let [head-pos (get g head)
        tail-pos (get g tail)
        step (matrix/add tail-pos (map #(int (math/signum %)) (matrix/sub head-pos tail-pos)))]
    (visit g tail step)))

(defn knot-catchup [g knot prev-knot]
  (if (grid/adjacent? :compass-self (get g prev-knot) (get g knot))
    g
    (step-towards g prev-knot knot)))

(defn move-knots [{:keys [knots] :as g}]
  (->> knots
       (reduce (fn [[g prev-knot] knot]
                 [(knot-catchup g knot prev-knot) knot]) [g :head])
       first))

(defn apply-move [g move]
  (let [head-steps (grid/walk (get g :head) (grid/step-moves [move]))]
    (reduce (fn [g step]
              (-> g
                  (visit :head step)
                  (move-knots))) g head-steps)))

(defn execute-moves [g moves]
  (if-let [move (grid/parse-move (first moves))]
    (recur (apply-move g move)
           (rest moves))
    g))

(defn make-grid [num-knots]
  (let [knots (concat [:head]
                      (map #(keyword (str "knot-" %))
                           (range 1 (- num-knots 1)))
                      [:tail])
        g {:knots (rest knots)}]
    (reduce #(visit %1 %2 [0 0]) g knots)))

(defn solve [fname num-knots]
  (->>
   (read-input-lines fname)
   (execute-moves (make-grid num-knots))
   (vals)
   (filter :tail)
   count))

(defn part1 [fname]
  (solve fname 2))

(defn part2 [fname]
  (solve fname 10))

(part1 "2022/day09-sample.txt")
;; => 13

(part1 "2022/day09.txt")
;; => 6464

(part2 "2022/day09-sample2.txt")
;; => 36

(part2 "2022/day09.txt")
;; => 2604
