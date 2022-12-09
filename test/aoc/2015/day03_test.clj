(ns aoc.2015.day03-test
  (:require
   [aoc.2015.day03 :as sut]
   [clojure.test :refer :all]))

(deftest challenges
  (is (= 2572 (sut/part-1 sut/input)))
  (is (= 2631 (sut/part-2 sut/input))))
