(ns aoc.2023.day04
  (:require [aoc.core :refer [read-input-lines  pull-ints]]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn parse-line [line]
  (let [rest (str/split line #":")
        card-nr (-> (first rest) pull-ints last)
        [winning-str having-str] (str/split (second rest) #"\|")
        winning (into #{} (pull-ints winning-str))
        having (into #{} (pull-ints having-str))
        same (set/intersection winning having)]
    {:card-nr card-nr
     :same same
     :same-count (count same)}))

(defn calc-points [{:keys [same]}]
  (if (= 0 (count  same))
    0
    (apply * (repeat (dec (count same)) 2))))

(defn part-1 [fname]
  (->> (read-input-lines fname)
       (map parse-line)
       (map calc-points)
       (reduce +)))

(defn wins-copies-of
  "Given card number and number of matches for that card, returns a list of the card numbers we get copies of"
  [card-nr nr-matches]
  (range (inc card-nr) (+ 1 card-nr nr-matches)))

(defn add-copies-for
  "Given the cards and a list of new card numbers updates the running count. new-copies is a list of card numbers"
  [{:keys [cards-total] :as state} new-copies]
  (assoc state :cards-total
         (reduce (fn [cards-total new-copy-nr]
                   (update cards-total new-copy-nr inc)) cards-total new-copies)))

(defn wins-copies-reducing [{:keys [cards-start] :as cards} start-card-nr]
  (let [copies (wins-copies-of start-card-nr (cards-start start-card-nr))]
    (reduce
     (fn [cards copy]
       (wins-copies-reducing cards copy))
     (add-copies-for cards copies)
     copies)))

(defn part-2 [fname]
  (let [cards (->> (read-input-lines fname)
                   (map parse-line)
                   (map #(dissoc % :same))
                   (reduce (fn [acc card]
                             (-> acc
                                 (assoc-in  [:cards-start (:card-nr card)] (:same-count card))
                                 (assoc-in [:cards-total (:card-nr card)] 1)))
                           {;; card-nr -> # matches
                            :cards-start {}
                            ;; card-nr -> total number of that card
                            :cards-total {}}))]
    (->> (range 1 (inc (count (:cards-start cards))))
         (reduce #(wins-copies-reducing %1 %2) cards)
         :cards-total
         vals
         (reduce +))))

(comment
  (part-1 "2023/day04-sample.txt") ;; rcf
  ;; => 13

  (part-1 "2023/day04.txt") ;; rcf
  ;; => 33950

  (part-2 "2023/day04-sample.txt") ;; rcf
  ;; => 30

  (part-2 "2023/day04.txt") ;; rcf
  ;; => 14814534
  )
