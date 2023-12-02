(ns aoc.2023.day02-test
  (:require [aoc.2023.day02 :as sut]
            [clojure.test :refer :all]))

(def sample "2023/day02-sample.txt")
(def input "2023/day02.txt")

(deftest day02
  (is (= 8 (sut/part-1 sample)))
  (is (= 2632 (sut/part-1 input)))
  (is (= 2286 (sut/part-2 sample)))
  (is (= 69629 (sut/part-2 input))))
