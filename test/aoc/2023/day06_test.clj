(ns aoc.2023.day06-test
  (:require [aoc.2023.day06 :as sut]
            [clojure.test :refer :all]))

(def sample "2023/day06-sample.txt")
(def input "2023/day06.txt")
(deftest day06
  (is (= 288  (sut/part-1 sample)))
  (is (= 275724 (sut/part-1 input)))
  (is (= 71503 (sut/part-2 sample)))
  (is (= 37286485 (sut/part-2 input)))
  )
