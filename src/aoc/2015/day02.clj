(ns aoc.2015.day02
  (:require
   [aoc.core :as core :refer [read-input read-input-lines]]))

(def input (read-input-lines "2015/day02.txt"))
(def sample "2x3x4")

(defn surface-area [[l w h]]
  (+ (* 2 l w)
     (* 2 w h)
     (* 2 h l)))

(defn smallest-side-area [[l w h]]
  (min (* l w) (* l h) (* w h)))

(defn paper-calc [dim]
  (+ (surface-area dim)
     (smallest-side-area dim)))

(defn smallest-perimeter [[l w h]]
  (min (+ (* 2 w) (* 2 l))
       (+ (* 2 w) (* 2 h))
       (+ (* 2 h) (* 2 l))))

(defn calc-ribbon [dim]
  (+ (smallest-perimeter dim)
     (reduce * dim)))

(defn part-1 [input]
  (->>
   (map core/pull-ints input)
   (map paper-calc)
   (reduce +)))

(part-1 input)
;; => 1606483

(defn part-2 [input]
  (->>
   (map core/pull-ints input)
   (map calc-ribbon)
   (reduce +)))

(part-2 input)
;; => 3842356
