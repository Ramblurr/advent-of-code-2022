(ns aoc.2023.day01
  (:require [aoc.core :refer [read-input-lines map-all pull-digits re-pos]]
            [medley.core :as m]
            [clojure.string :as str]))

(defn part-1 [fname]
  (->>
   (read-input-lines fname)
   (map str/split-lines)
   (map-all pull-digits)
   (map flatten)
   (map #(str (first %) (last %)))
   (map #(Long/parseLong %))
   (reduce +)))

(part-1 "2023/day01-sample.txt")        ;; rcf
;; => 142

(part-1 "2023/day01.txt") ;; rcf
;; => 55108

(def digits #{"0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
              "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "zero"})

(def fix-digits {"0" 0 "1" 1 "2" 2 "3" 3 "4" 4 "5" 5 "6" 6 "7" 7 "8" 8 "9" 9
                 "one" 1 "two" 2 "three" 3 "four" 4 "five" 5 "six" 6 "seven" 7 "eight" 8 "nine" 9 "zero" 0})

(defn position-of-digits [s]
  (->> digits
       (reduce (fn [acc jank-digit]
                 (merge acc (re-pos (re-pattern jank-digit) s)))
               {})
       (m/map-vals fix-digits)))

(defn to-first-last [s]
  (let [pos (position-of-digits s)
        sorted (->> pos (keys) (sort) (map #(pos %)))]
    (str  (first sorted) (last sorted))))

(defn part-2 [fname]
  (->>
   (read-input-lines fname)
   (map str/split-lines)
   (map-all to-first-last)
   flatten
   (map #(Long/parseLong %))
   (reduce +)))

(part-2 "2023/day01-sample2.txt")        ;; rcf
;; => 281

(part-2 "2023/day01.txt")        ;; rcf
;; => 56324
