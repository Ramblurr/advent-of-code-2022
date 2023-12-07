
(ns aoc.2023.day07-test
  (:require [aoc.2023.day07 :as sut]
            [clojure.test :refer :all]))

(def sample "2023/day07-sample.txt")
(def input "2023/day07.txt")

(deftest day07
  (is (= 6440 (sut/part-1 sample)))
  (is (= 251806792 (sut/part-1 input)))
  (is (= 5905 (sut/part-2 sample)))
  (is (= 252113488 (sut/part-2 input)))
  )
