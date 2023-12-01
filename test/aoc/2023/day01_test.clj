(ns aoc.2023.day01-test
  (:require [aoc.2023.day01 :as sut2]
            [clojure.test :refer :all]))

(def sample "2023/day01-sample.txt")
(def sample2 "2023/day01-sample2.txt")
(def input "2023/day01.txt")
(deftest day01
  (is (= 142 (sut2/part-1 sample)))
  (is (= 55108 (sut2/part-1 input)))
  (is (= 281 (sut2/part-2 sample2)))
  (is (= 56324 (sut2/part-2 input))))
