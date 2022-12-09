(ns aoc.{{year}}.day{{day}}-test
    (:require
     [aoc.{{year}}.day{{day}} :as sut]
     [clojure.test :refer :all]))

(deftest challenges
  (is (= false (sut/part-1 sut/input)))
  (is (= false (sut/part-2 sut/input))))
