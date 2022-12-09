(ns aoc.2015.day04-test
  (:require
   [aoc.2015.day04 :as sut]
   [clojure.test :refer :all]))

(def sample "abcdef")
(deftest challenges
  (is (= 609043 (sut/part-1 sample)))
  (is (= 346386 (sut/part-1 sut/input)))
  (is (= 9958218 (sut/part-2 sut/input))))
