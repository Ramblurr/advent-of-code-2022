(ns aoc.2023.day04-test
  (:require [aoc.2023.day04 :as sut]
            [clojure.test :refer :all]))


(def sample "2023/day04-sample.txt")
(def input "2023/day04.txt")
(deftest day04
  (is (= 13  (sut/part-1 sample)))
  (is (= 33950  (sut/part-1 input)))
  (is (= 30  (sut/part-2 sample)))
  (is (= 14814534  (sut/part-2 input)))
  )
