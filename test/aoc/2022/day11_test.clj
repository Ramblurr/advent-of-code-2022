(ns aoc.2022.day11-test
  (:require
   [aoc.2022.day11 :as sut]
   [clojure.test :refer :all]))

(deftest challenges
  (is (= 10605 (sut/part-1 sut/sample)))
  (is (= 118674 (sut/part-1 sut/input)))
  (is (= 2713310158 (sut/part-2 sut/sample)))
  (is (= 32333418600 (sut/part-2 sut/input))))
