(ns aoc.2023.day08
  (:require [aoc.core :refer [read-input]]
            [medley.core :as m]
            [clojure.string :as str]))

(defn parse-map [input]
  (->>
   (str/split-lines input)
   (map (fn [line]
          (let [[_ node left right] (re-find (re-matcher #"([A-Z1-9]{3}) = \(([A-Z1-9]{3}), ([A-Z1-9]{3})\)" line))]
            {:node node
             \L left
             \R right})))
   (group-by :node)
   (m/map-vals first)))

(defn parse [fname]
  (let [input (read-input fname)
        [dirs map'] (str/split input #"\n\n")]
    {:dirs (seq dirs)
     :map' (parse-map map')}))

(defn take-step [map' current dir]
  (get (get map' current) dir))

(defn walk [map' dirs start end?]
  (reduce
   (fn [{:keys [step current] :as state} dir]
     (if (or (>= step 100000) (end? current))
       (reduced (:step state))
       (-> state
           (update :step inc)
           (update :current #(take-step map' % dir)))))
   {:step 0 :current start}
   (cycle dirs)))

(defn part-1 [fname]
  (let [{:keys [map' dirs]} (parse fname)]
    (walk map' dirs "AAA" #(= "ZZZ" %))))

(defn gcd [x y]
  (if (zero? y)
    x
    (gcd y (mod x y))))

(defn lcm [x y]
  (/ (* x y) (gcd x y)))

(def start-A? #(= \A (last %)))

(def last-Z? #(= \Z (last %)))

(defn part-2 [fname]
  (let [{:keys [map' dirs]} (parse fname)
        starting-nodes (filter start-A? (map :node (vals map')))]
    (->> starting-nodes
         (map #(walk map' dirs % last-Z?))
         (reduce lcm))))

(comment
  (parse "2023/day08-sample.txt")   ;; rcf
  (part-1 "2023/day08-sample.txt")  ;; rcf
  ;; => 2
  (part-1 "2023/day08-sample2.txt") ;; rcf
  ;; => 6
  (part-1 "2023/day08.txt")         ;; rcf
  ;; => 13301
  (part-2 "2023/day08-sample3.txt") ;; rcf
  ;; => 6
  (part-2 "2023/day08.txt") ;; rcf
  ;; => 7309459565207
  )
