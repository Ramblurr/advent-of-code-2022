(ns aoc.2022.day10-test
  (:require
   [aoc.2022.day10 :as sut]
   [clojure.test :refer :all]))

(deftest challenges
  (is (= 13140 (sut/part-1 sut/sample)))
  (is (= 12560 (sut/part-1 sut/input)))
  (is (=
       '("###..#....###...##..####.###...##..#...."
         "#..#.#....#..#.#..#.#....#..#.#..#.#...."
         "#..#.#....#..#.#..#.###..###..#....#...."
         "###..#....###..####.#....#..#.#....#...."
         "#....#....#....#..#.#....#..#.#..#.#...."
         "#....####.#....#..#.#....###...##..####.")
       (sut/part-2 sut/input))))
