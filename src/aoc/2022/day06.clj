(ns aoc.2022.day06
  (:require
   [aoc.core :refer [read-input-lines]]))

(def s1 "mjqjpqmgbljsphdztnvjfqwrcgsmlb")
(def s2 "nppdvjthqldpwncqszvftbrmjlhg")
(def s3 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")
(def s4 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")

(defn base [n in]
  (->> in
       (partition n 1)
       (take-while #(not (apply distinct? %)))
       (count)
       (+ n)))

(base 4 s1)
;; => 7
(base 4 s2)
;; => 6
(base 4 s3)
;; => 10
(base 4 s4)
;; => 11

(defn part1 [fname]
  (->> (read-input-lines fname)
       first
       (base 4)))

(part1 "2022/day06.txt")
;; => 1300

(defn part2 [fname]
  (->> (read-input-lines fname)
       first
       (base 14)))

(part2 "2022/day06.txt")
;; => 3986
