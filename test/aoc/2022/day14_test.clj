(ns aoc.2022.day14-test
  (:require
   [aoc.2022.day14 :as sut]
   [clojure.test :refer :all]))

(deftest challenges
  (is (= 24 (sut/part-1 sut/sample)))
  (is (= 618 (sut/part-1 sut/input)))

  (is (= 93 (sut/part-2 sut/sample)))
  (is (= 26358 (sut/part-2 sut/input))))
