(ns aoc.2022.day15-test
    (:require
     [aoc.2022.day15 :as sut]
     [clojure.test :refer :all]))

(deftest challenges
  (is (= false (sut/part-1 sut/input)))
  (is (= false (sut/part-2 sut/input))))
