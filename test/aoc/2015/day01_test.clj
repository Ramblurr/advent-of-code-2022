(ns aoc.2015.day01-test
  (:require
   [aoc.2015.day01 :as sut]
   [clojure.test :refer :all]))

(deftest challenges
  (is (= 0 (sut/part-1 "(())")))
  (is (= 3 (sut/part-1 "(((")))
  (is (= -3 (sut/part-1 ")())())")))
  (is (= 232 (sut/part-1 sut/input))))

(deftest part-2
  (is (= 5 (sut/part-2 "()())")))
  (is (= 1783 (sut/part-2 sut/input))))
