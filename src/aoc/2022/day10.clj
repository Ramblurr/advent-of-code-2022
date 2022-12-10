(ns aoc.2022.day10
  (:refer-clojure :exclude [parse-long])
  (:require
   [aoc.core :as core :refer [read-input-lines parse-long]]
   [clojure.string :as str]
   [medley.core :as m]))

(def sample (read-input-lines "2022/day10-sample.txt"))
(def input (read-input-lines "2022/day10.txt"))

(defn signal-strength [{:keys [X cycle] :as state}]
  (assoc state :signal-strength (* X cycle)))

(defn step-cycle
  "Step a single machine cycle"
  [f {:keys [context instrs] :as state} clock]
  (let [[op arg] (first instrs)]
    (cond-> state
      true (update :cycle inc)
      (some? context) (->
                       f
                       (dissoc :context)
                       (update :X + context))
      (nil? context) (cond->
                      true f
                      true (update :instrs rest)
                      (= :addx op) (assoc :context arg)))))

(defn execute [f instructions]
  (->> (range)
       (reductions (partial step-cycle f) {:X 1
                                           :cycle 0
                                           :context nil
                                           :instrs instructions
                                           :display []})
       (m/take-upto #(empty? (:instrs %)))))

(defn parse [input]
  (->> input
       (map #(str/split % #" "))
       (map #(-> % (update 0 keyword)
                 (update 1 parse-long)))))

(defn part-1 [input]
  (->> (parse input)
       (execute signal-strength)
       (filter #(#{20 60 100 140 180 220} (:cycle %)))
       (map :signal-strength)
       (reduce +)))

;; (part-1 sample)
;; => 13140
;; (part-1 input)
;; => 12560

(defn draw-pixel [{:keys [X cycle] :as state}]
  (update state :display conj
          (if (<= -1 (- X (mod (dec cycle) 40)) 1)
            "#"
            ".")))

(defn part-2 [input]
  (->> (parse input)
       (execute draw-pixel)
       last
       :display
       (partition 40)
       (map (partial apply str))))

;; (part-2 input) ;; PLPAFBCL
