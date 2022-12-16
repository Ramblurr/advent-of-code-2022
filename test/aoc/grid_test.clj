(ns aoc.grid-test
  (:require
   [aoc.grid :as grid]
   [clojure.test :refer :all]))

(deftest slope
  (is (= 2 (grid/slope [1 3] [3 7]))))

(deftest enumerate-line
  (is (= '([0 0] [1 1] [2 2] [3 3] [4 4] [5 5])
         (grid/enumerate-line [0 0] [5 5])))
  (is (= '([0 0] [0 1] [0 2] [0 3] [0 4] [0 5])
         (grid/enumerate-line [0 0] [0 5])))
  (is (= '([0 0] [1 0] [2 0] [3 0] [4 0] [5 0])
         (grid/enumerate-line [0 0] [5 0])))
  (is (= '([496 6] [497 6] [498 6])
         (grid/enumerate-line [498 6] [496 6]))))

(deftest bounds
  (is (= [[0 0] [0 0]] (grid/bounds {[0 0] :_})))
  (is (= [[0 0] [1 1]] (grid/bounds {[0 0] :_ [1 1] :_}))))

(deftest manhattan-poly
  (is (=  [[8 -2] [17 7] [8 16] [-1 7]]
          (grid/manhattan-poly [8 7] 9))))
