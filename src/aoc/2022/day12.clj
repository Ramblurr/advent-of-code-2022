(ns aoc.2022.day12
  (:require
   [loom.graph :as loom]
   [loom.alg :as loom-alg]
   [aoc.grid :as grid]
   [aoc.core :as core :refer [read-input-lines]]
   [medley.core :as m]))

(def sample (read-input-lines "2022/day12-sample.txt"))
(def input (read-input-lines "2022/day12.txt"))

(defn parse-cell [v]
  (condp = v
    "E" {:height (int \z) :end true}
    "S" {:height (int \a)  :start true}
    {:height (int (first v))}))

(defn can-walk? [from-h to-h]
  (when to-h
    (or (= (inc from-h) to-h)
        (<= to-h from-h))))

(defn terminal-node [g kw]
  (first (m/find-first (fn [[pos v]]
                         (when (get v kw) pos)) g)))

(defn has-edge? [g from to]
  (let [from-h (get-in g [from :height])
        to-h (get-in g [to :height])]
    (can-walk? from-h to-h)))

(defn solve [g start-nodes end]
  (let [graph (loom/digraph (grid/adjacency-graph grid/adjacent-cardinal has-edge?  g))]
    (->> start-nodes
         (map #(-> graph
                   (loom-alg/bf-path % end)
                   (rest)
                   (count)))
         (filter #(> % 0))
         (apply min))))

(defn part-1 [input]
  (let [g (grid/to-grid input parse-cell)]
    (solve  g
            #{(terminal-node g :start)}
            (terminal-node g :end))))

(defn part-2 [input]
  (let [g (grid/to-grid input parse-cell)
        lowest-coords (keys (m/filter-vals #(= (int \a) (:height %)) g))]
    (solve g
           lowest-coords
           (terminal-node g :end))))

(comment
  (part-1 input)
  ;; => 380
  (part-2 sample)
  ;; => 29
  (part-2 input)
  ;; => 375
  )
