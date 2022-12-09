(ns aoc.2015.day02-test
  (:require
   [aoc.2015.day02 :as sut]
   [clojure.test :refer :all]))

(deftest challenges
  (is (= 1606483 (sut/part-1 sut/input)))
  (is (= 3842356 (sut/part-2 sut/input))))
