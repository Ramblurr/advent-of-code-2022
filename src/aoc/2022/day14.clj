(ns aoc.2022.day14
  (:require
   [clojure.string :as str]
   [aoc.grid :as grid]
   [aoc.core :as core :refer [read-input-lines]]
   [medley.core :as m]))

(def sample (read-input-lines "2022/day14-sample.txt"))
(def input (read-input-lines "2022/day14.txt"))

(defn parse [input source inf-floor?]
  (let [rocks
        (->> (map (fn [line]
                    (read-string (str "[["
                                      (str/replace line #" -> " "][")
                                      "]]"))) input)
             (map (partial partition 2 1))
             (core/map-all #(apply grid/enumerate-line %))
             (apply concat)
             (apply concat))
        g (zipmap rocks (take (count rocks) (repeat {:rock true})))
        g (-> g
              (assoc :bounds (grid/bounds g))
              (assoc source {:source true})
              (assoc :source source))
        y-bottom (+ 2 (-> g :bounds second second))]
    (if inf-floor?
      (-> g
          (assoc :infinite-floor-y y-bottom)
          (assoc-in [:bounds 1 1] y-bottom))
      g)))

(defn add-sand [g p] (assoc-in g [p :sand] true))
(defn remove-sand [g p] (m/dissoc-in g [p :sand]))

(defn down-one [[x y]] [x (inc y)])
(defn down-left [[x y]] [(dec x) (inc y)])
(defn down-right [[x y]] [(inc x) (inc y)])
(defn can-move? [g pos]
  (and (not (get-in g [pos :sand]))
       (not (get-in g [pos :rock]))
       (if (:infinite-floor-y g)
         (not (>= (second pos) (:infinite-floor-y g))) true)))

(defn step-sand
  "Return the position to which the sand moves (if any)"
  [g sand-pos]
  (cond
    (can-move? g (down-one sand-pos)) (down-one sand-pos)
    (can-move? g (down-left sand-pos)) (down-left sand-pos)
    (can-move? g (down-right sand-pos)) (down-right sand-pos)
    :else nil))

(defn tick
  "Simulate the falling of a single grain of sand."
  [{:keys [source bounds] :as g} sand-pos tick-n]
  (let [g (add-sand g sand-pos)]
    (if-let [new-pos (step-sand g sand-pos)]
      (if (or (>= (second new-pos) (-> bounds second second))
              (= source new-pos))
        (assoc g :oob tick-n)
        (recur (-> g (remove-sand sand-pos) (add-sand new-pos)) new-pos tick-n))
      (if (= source sand-pos)
        (assoc g :oob (inc tick-n))
        g))))

(defn render-cell [g [_ y] {:keys [rock source sand]}]
  (cond rock   "#"
        sand   "o"
        source "+"
        (and (:infinite-floor-y g) (>= y (:infinite-floor-y g))) "#"
        :else  "."))

(defn render-cave [g]
  (tap> (grid/render-grid g render-cell))
  (tap> (format "Terminated after %d units of sand" (:oob g)))
  (:oob g))

(defn go [input part-2?]
  (loop [state (parse input [500 0] part-2?)
         iter-n 0]
    (let [new-state (tick state (:source state) iter-n)]
      (if (:oob new-state)
        new-state
        (recur new-state (inc iter-n))))))

(defn part-1 [input]
  (render-cave (go input false)))

(defn part-2 [input]
  (render-cave (go input true)))

(comment
  (part-1 sample)
  ;; => 24
  (part-1 input)
  ;; => 618

  ;; run a limited number of iterations
  (render-cave
   (reduce
    (fn [state i]
      (tick state (:source state) i))
    (parse sample [500 0] true)
    (range 100)))

  (part-2 sample)
  ;; => 93
  (time (part-2 input))
  ;; => 26358
  )
