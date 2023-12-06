(ns aoc.2023.day06
  (:require [aoc.core :refer [read-input-lines pull-ints pull-digits]]))

(defn parse [fname]
  (->>  (read-input-lines fname)
        (map  pull-ints)
        (apply zipmap) ;; time -> record distance
        ))

(defn solve-race [[time record-distance]]
  (->> (range time)
       (map (fn [seconds-held]
              (* seconds-held (- time seconds-held))))
       (filter #(> % record-distance))
       (count)))

(defn part-1 [fname]

  (->> (parse fname)
       (map solve-race)
       (reduce *)))

(defn parse2 [fname]
  (->>  (read-input-lines fname)
        (map  pull-digits)
        (map #(apply str %))
        (map (fn [v] [(parse-long v)]))
        (apply zipmap) ;; time -> record distance
        ))

(defn part-2 [fname]
  (->> (parse2 fname)
       (map solve-race)
       (reduce *)))

(comment
  (part-1 "2023/day06-sample.txt") ;; rcf
  ;; => 288
  (part-1 "2023/day06.txt") ;; rcf
  ;; => 275724
  (part-2 "2023/day06-sample.txt") ;; rcf
  ;; => 71503
  (part-2 "2023/day06.txt") ;; rcf
  ;; => 37286485
  ;;
  )
