(ns aoc.2023.day08-test
  (:require [aoc.2023.day08 :as sut]
            [clojure.test :refer :all]))

(def sample "2023/day08-sample.txt")
(def sample2 "2023/day08-sample2.txt")
(def input "2023/day08.txt")

(deftest day08
  (is (= 2 (sut/part-1 sample)))
  (is (= 6 (sut/part-1 sample2)))
  (is (= 13301 (sut/part-1 input)))
  (is (= 6 (sut/part-2 "2023/day08-sample3.txt")))
  (is (= 7309459565207 (sut/part-2 input)))
  )
