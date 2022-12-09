(ns aoc.2022.day03
  (:require [aoc.core :refer [read-input map-all reduce-all]]
            [medley.core :as m]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn split-in-half [s]
  (let [len (count s)]
    {:comp1
     (set  (first (partition (quot len 2) s)))
     :comp2
     (set (second (partition (quot len 2) s)))}))

(defn in-common [{:keys [comp1 comp2] :as ruck}]
  (assoc ruck :in-common
         (set/intersection comp1 comp2)))

(def upper-case-letters
  (map char (concat (range 65 91))))
(def lower-case-letters
  (map char (concat  (range 97 123))))

(def priorities
  (merge
   (zipmap lower-case-letters (range 1 27))
   (zipmap upper-case-letters (range 27 53))))

(defn prioritize [{:keys [in-common] :as rucksack}]
  (assert (= 1 (count in-common)))
  (-> rucksack
      (assoc :prio  (->> (map #(get priorities %) in-common)
                         (reduce +)))))

(defn part1 [fname]
  (->>
   (-> (read-input fname)
       (str/split-lines))
   (map split-in-half)
   (map in-common)
   (map prioritize)
   (map :prio)
   (reduce +)))

(part1 "day03-sample.txt")
(part1 "day03.txt")

(defn to-set [[g1 g2 g3]]
  {:g1 (into #{} g1)
   :g2 (into #{} g2)
   :g3 (into #{} g3)})

(defn in-common2 [{:keys [g1 g2 g3] :as g}]
  (assoc g :in-common
         (set/intersection g1 g2 g3)))

(defn part2 [fname]
  (->>
   (-> (read-input fname)
       (str/split-lines))
   (partition 3)
   (map to-set)
   (map in-common2)
   (map prioritize)
   (map :prio)
   (reduce +)))

(part2 "day03-sample.txt")
(part2 "day03.txt")
