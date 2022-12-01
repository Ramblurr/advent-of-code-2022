(ns aoc.core

  (:require [clojure.java.io :as io]))

(defn read-input
  [day]
  (slurp (io/resource day)))

(defn map-all [f coll]
  (map (fn [v] (map f v)) coll))

(defn reduce-all [f val coll]
  (map (fn [v] (reduce f val v)) coll))
