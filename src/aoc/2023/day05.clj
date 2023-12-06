(ns aoc.2023.day05
  (:require [aoc.core :refer [read-input pull-ints]]
            [clojure.string :as str]))

(defn parse-section [section]
  (if (str/includes? section "seeds: ")
    (into [] (pull-ints section))
    (let [[_ from to] (re-find (re-matcher  #"([a-z]+)-to-([a-z]+) map:" section))
          def (->> section
                   (str/split-lines)
                   (drop 1)
                   (map pull-ints)
                   (map (fn [[dest-r source-r length]]
                          {:dest-r dest-r
                           :range [dest-r source-r length]
                           :source-r source-r
                           :length length})))]

      {:source (keyword from) :dest (keyword to)
       :ranges  (map :range def)
       :def def})))

;; brute force functions for part-1, these are too inefficient for part-2
(defn map-lookup [{:keys [dest-r source-r length]} source-v]
  (if (or (< source-v source-r) (>= source-v (+ source-r length)))
    nil
    (let [offset (- source-v source-r)]
      (+ dest-r offset))))

(defn maps-lookup [maps source-v]
  (or
   (reduce (fn [result map]
             (if (nil? result)
               (map-lookup map source-v)
               (reduced result))) nil maps)
   source-v))

(map-lookup {:dest-r 50 :source-r 98 :length 2} 79)
;; => nil
(map-lookup {:dest-r 52 :source-r 50 :length 48} 79)
;; => 81

(maps-lookup
 [{:dest-r 50 :source-r 98 :length 2}
  {:dest-r 52 :source-r 50 :length 48}] 79)
;; => 81

(defn process [maps seed]
  (reduce (fn [current {:keys [def]}]
            (let [r (maps-lookup def current)]
              r))
          seed
          maps))

(defn part-1 [fname]
  (let [sections (-> (read-input fname)
                     (str/split #"\n\n"))
        sections (map parse-section sections)
        seeds (first sections)
        maps (rest sections)]
    (apply min (map (partial process maps) seeds))))

(defn limit-range
  [f s l]
  (some-> (when (pos? l) [s l])
          (f)
          (vector)))

(defn solve-range [seed-range [dst src len]]
  (if (reduced? seed-range)
    [seed-range]
    (let [[seed-start seed-length] seed-range
          seed-end (+ seed-start seed-length)
          src-end (+ src len)]
      (concat
        ;; left
       (limit-range identity seed-start (min seed-length (- src seed-start)))
       ;; middle
       (let [is (max src seed-start)
             ie (min seed-end src-end)]
         (limit-range reduced (+ dst (- is src)) (min len (- ie is))))
       ;; right
       (let [i (max src-end seed-start)]
         (limit-range identity i (- seed-end i)))))))

(solve-range [79 14] [50 98 2])
;; => ([79 14])
(map unreduced (solve-range [79 14] [52 50 48]))
;; => ([81 14])

(defn solve-map [seed-range ranges]
  (->> (reduce (fn [seed-range range] (mapcat #(solve-range % range) seed-range))
               [seed-range]
               ranges)
       (map unreduced)))

(solve-map [79 14] [[50 98 2] [52 50 48]])
;; => ([81 14])

(defn solve-to-location [seed-ranges maps]
  (tap> {:maps maps})
  (->>
   (reduce (fn [seed-ranges map]
             (mapcat #(solve-map % map) seed-ranges))
           [seed-ranges]
           maps)
   (map first)))

(defn prepare-2 [fname seed-fn]
  (let [sections (-> (read-input fname)
                     (str/split #"\n\n"))
        sections (map parse-section sections)
        seed-ranges (seed-fn (first sections))
        maps (map :ranges (rest sections))]
    [maps seed-ranges]

    [maps seed-ranges]))

(defn part-2 [fname]
  (let [[maps seed-ranges] (prepare-2 fname #(partition 2 %))]
    (->>
     (mapcat #(solve-to-location % maps) seed-ranges)
     (apply min))))

(defn part-1-with-solution-2 [fname]
  (let [[maps seed-ranges] (prepare-2 fname #(map (fn [seed] [seed 1]) %))]
    (->>
     (mapcat #(solve-to-location % maps) seed-ranges)
     (apply min))))

(comment
  (part-1 "2023/day05-sample.txt") ;; rcf
  ;; => 35
  (part-1 "2023/day05.txt") ;; rcf
  ;; => 825516882
  (part-2 "2023/day05-sample.txt") ;; rcf
  ;; => 46
  (part-2 "2023/day05.txt") ;; rcf
  ;; => 136096660

  (part-1-with-solution-2 "2023/day05-sample.txt") ;; rcf
  ;; => 35
  (part-1-with-solution-2 "2023/day05.txt") ;; rcf
  ;; => 825516882
  ;;
  )
