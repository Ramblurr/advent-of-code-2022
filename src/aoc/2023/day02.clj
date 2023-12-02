(ns aoc.2023.day02
  (:require [aoc.core :refer [read-input-lines  pull-ints]]
            [clojure.string :as str]))

(def p1-constraints {:red 12
                     :green 13
                     :blue 14})

(defn p1-parse-line [line]
  (let [rest (str/split line #":")
        game-nr (-> (first rest) pull-ints last)
        reveals (->> (str/split (second rest) #"[,;]")
                     (map str/trim)
                     (map #(str/split % #" "))
                     (reduce (fn [acc [nr color]]
                               (let [revealed (Integer/parseInt nr)
                                     color (keyword color)]
                                 (update acc color #(max (or % 0) revealed))))
                             {}))]

    {:game game-nr :max-reveals reveals :game-satisfied? true}))

(defn p1-stats [game]
  (reduce (fn [game [constraint-color constraint]]
            (let [satisfied?  (<= (get-in game [:max-reveals constraint-color] 0) constraint)]
              (-> game
                  (assoc-in  [:satisfied constraint-color] satisfied?)
                  (update :game-satisfied? #(and % satisfied?))))) game p1-constraints))

(defn part-1 [fname]
  (->>
   (read-input-lines fname)
   (map p1-parse-line)
   (map p1-stats)
   (map (fn [{:keys [game game-satisfied?]}]
          (if  game-satisfied? game 0)))
   (reduce +)))

(part-1 "2023/day02-sample.txt")        ;; rcf
;; => 8

(part-1 "2023/day02.txt")        ;; rcf
;; => 2632

(defn part-2 [fname]
  (->>
   (read-input-lines fname)
   (map p1-parse-line)
   (map (fn [{:keys [max-reveals]}]
          (reduce * (vals max-reveals))))
   (reduce +)))

(part-2 "2023/day02-sample.txt")        ;; rcf
;; => 2286

(part-2 "2023/day02.txt")        ;; rcf
;; => 69629
