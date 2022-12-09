(ns aoc.2022.day01-test
  (:require [aoc.2022.day01 :as sut]
            [clojure.test :refer :all]))

(def sample "2022/day01-sample.txt")
(def input "2022/day01.txt")
(deftest day01
  (is (= 24000 (sut/part-1 sample)))
  (is (= 67622 (sut/part-1 input)))
  (is (= 45000 (sut/part-2 sample)))
  (is (= 201491 (sut/part-2 input))))
