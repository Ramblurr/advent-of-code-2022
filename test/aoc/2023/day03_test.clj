(ns aoc.2023.day03-test
  (:require [aoc.2023.day03 :as sut]
            [clojure.test :refer :all]))

(def sample "2023/day03-sample.txt")
(def input "2023/day03.txt")
(deftest day02
  (is (= 4361 (sut/part-1 sample)))
  (is (= 512794 (sut/part-1 input)))
  (is (= 467835 (sut/part-2 sample)))
  (is (= 67779080 (sut/part-2 input))))
