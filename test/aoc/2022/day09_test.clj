(ns aoc.2022.day09-test
  (:require [aoc.2022.day09 :as sut]
            [clojure.test :refer :all]))

(def sample "2022/day09-sample.txt")
(def sample2 "2022/day09-sample2.txt")
(def input "2022/day09.txt")
(deftest day09
  (is (=  13 (sut/part1 sample)))
  (is (=  6464 (sut/part1 input)))
  (is (=  36 (sut/part2 sample2)))
  (is (=  2604 (sut/part2 input))))
