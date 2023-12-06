(ns aoc.2023.day05-test
  (:require [aoc.2023.day05 :as sut]
            [clojure.test :refer :all]))

(def sample "2023/day05-sample.txt")
(def input "2023/day05.txt")
(deftest day05
  (is (= 35 (sut/part-1 sample)))
  (is (= 825516882 (sut/part-1 input)))
  (is (= 46  (sut/part-2 sample)))
  (is (= 136096660 (sut/part-2 input)))
  (is (= 35 (sut/part-1-with-solution-2 sample)))
  (is (= 825516882 (sut/part-1-with-solution-2 input))))
