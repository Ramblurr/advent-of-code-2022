(ns aoc.2022.day13-test
  (:require
   [aoc.2022.day13 :as sut]
   [clojure.test :refer :all]))

(deftest challenges
  (is (= 13 (sut/part-1 sut/sample)))
  (is (= 6235 (sut/part-1 sut/input)))
  (is (= 140 (sut/part-2 sut/sample)))
  (is (= 22866 (sut/part-2 sut/input))))
