(ns aoc.2023.day07
  (:require [aoc.core :refer [read-input-lines]]
            [clojure.string :as str]))

(def kind-values {:high-card 1
                  :one-pair 2
                  :two-pair 3
                  :three-of-a-kind 4
                  :full-house 5
                  :four-of-a-kind 6
                  :five-of-a-kind 7})

(def cards-value {\2 2
                  \3 3
                  \4 4
                  \5 5
                  \6 6
                  \7 7
                  \8 8
                  \9 9
                  \T 10
                  \J 11
                  \Q 12
                  \K 13
                  \A 14})

(defn hand-kind [hand]
  (let [frequency (vals  (frequencies hand))
        counts (set frequency)]
    (cond
      (counts 5) :five-of-a-kind
      (counts 4) :four-of-a-kind
      (and (counts 3) (counts 2)) :full-house
      (counts 3) :three-of-a-kind
      (and (counts 2) (= 2 (count  (filter #(= 2 %) frequency)))) :two-pair
      (counts 2) :one-pair
      :else :high-card)))

(defn hand-score-key [cards-value {:keys [hand kind] :as h}]
  [(kind-values kind) (mapv cards-value hand)])

(defn parse [fname hand-kind cards-value]
  (->>  (read-input-lines fname)
        (map #(str/split % #" "))
        (map (fn [[hand bid]]
               {:hand hand
                :bid (parse-long bid)
                :kind (hand-kind hand)}))
        (map (fn [hand]
               (assoc hand :score-key (hand-score-key cards-value hand))))))

(defn part-1 [fname]
  (->>  (parse fname hand-kind cards-value)
        (sort-by :score-key)
        (map-indexed #(assoc %2 :rank (inc %1)))
        (map #(* (:rank %) (:bid %)))
        (reduce +)))

(def cards-value-pt2 (assoc cards-value \J 1))

(def joker-kind-tweak {0 identity
                       1 {:four-of-a-kind :five-of-a-kind
                          :three-of-a-kind :four-of-a-kind
                          :two-pair :full-house
                          :one-pair :three-of-a-kind
                          :high-card :one-pair}
                       2 {:three-of-a-kind :five-of-a-kind
                          :one-pair :four-of-a-kind
                          :high-card :three-of-a-kind}
                       3 {:one-pair :five-of-a-kind
                          :high-card :four-of-a-kind}
                       4 (constantly :five-of-a-kind)
                       5 (constantly :five-of-a-kind)})

(defn hand-kind-with-jokers [hand]
  (let [hand-less-jokers (remove #(= \J %) hand)
        normal-kind (hand-kind hand-less-jokers)
        joker-count (- (count hand) (count hand-less-jokers))]
    ((joker-kind-tweak joker-count) normal-kind)))

(defn part-2 [fname]
  (->>  (parse fname hand-kind-with-jokers cards-value-pt2)
        #_(map (fn [{:keys [kind] :as hand}]
                 (if (nil? kind)
                   (throw (ex-info "nil kind" hand))
                   hand)))
        (sort-by :score-key)
        (map-indexed #(assoc %2 :rank (inc %1)))
        (map #(* (:rank %) (:bid %)))
        (reduce +)))

(comment
  (part-1 "2023/day07-sample.txt") ;; rcf
  ;; => 6440

  (part-1 "2023/day07.txt")
  ;; => 251806792

  (part-2 "2023/day07-sample.txt") ;; rcf
  ;; => 5905
  (part-2 "2023/day07.txt")
  ;; => 252113488
  )
