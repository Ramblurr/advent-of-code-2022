(ns aoc.2022.day13
  (:require
   [aoc.core :as core :refer [read-input]]))

(def sample (read-input "2022/day13-sample.txt"))
(def input (read-input "2022/day13.txt"))

(defn parse [input]
  (read-string (str "[" input "]")))

(defn compare? [left right]
  (cond
    (and (int? left) (int? right))
    (cond (= left right) :continue
          (< left right) :right-order
          :else          :wrong-order)

    (and (coll? left) (coll? right))
    (loop [l  (first left) r  (first right)
           ll (rest left)  rr (rest right)]
      (cond (and (nil? l) (nil? r)) :continue
            (nil? l)                :right-order
            (nil? r)                :wrong-order
            :else                   (let [result (compare? l r)]
                                      (if (= :continue result)
                                        (recur (first ll) (first rr)
                                               (rest ll) (rest rr))
                                        result))))
    :else (compare? (core/ensure-vector left) (core/ensure-vector right))))

(defn part-1 [input]
  (->>
   (parse input)
   (partition 2)
   (map #(compare? (first %) (second %)))
   (map-indexed (fn [idx  r] (if (= :right-order r)
                               (inc idx)
                               0)))
   (reduce +)))

(def divider-packets #{[[2]] [[6]]})

(defn part-2 [input]
  (->> (parse input)
       (concat divider-packets)
       (sort #(= :right-order (compare? %1 %2)))
       (map-indexed (fn [idx v]
                      (if (divider-packets v)
                        (inc idx)
                        1)))
       (reduce *)))

(comment
  (part-1 sample)
  (part-1 input)
  ;; => 6235
  (part-2 sample)
  (part-2 input)
  ;; => 22866
  )
