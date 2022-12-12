(ns aoc.2022.day08-test
  (:require [aoc.2022.day08 :as sut]
            [clojure.test :refer :all]))

(deftest day08

  (is (= 21 (sut/part1 "2022/day08-sample.txt")))
  (is (= 1807 (sut/part1 "2022/day08.txt")))
  (is (= 8 (sut/part2 "2022/day08-sample.txt")))
  (is (= 480000 (sut/part2 "2022/day08.txt"))))
