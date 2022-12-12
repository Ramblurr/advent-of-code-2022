(ns aoc.2022.day12-test
  (:require
   [aoc.2022.day12 :as sut]
   [clojure.test :refer :all]))

(deftest challenges
  (is (= 31 (sut/part-1 sut/sample)))
  (is (= 380 (sut/part-1 sut/input)))
  (is (= 29 (sut/part-2 sut/sample)))
  (is (= 375 (sut/part-2 sut/input))))
