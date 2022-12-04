(ns day-02
  (:require [aoc.core :refer [read-input map-all reduce-all]]
            [medley.core :as m]
            [clojure.string :as str]
            [clojure.set :as set]))

(def codebook {"A" :rock
               "B" :paper
               "C" :scissors
               "X" :rock
               "Y" :paper
               "Z" :scissors})

(def roshambo {:rock :scissors
               :paper :rock
               :scissors :paper})

(def roshambo-inv (set/map-invert roshambo))

(def scorebook {:rock 1
                :paper 2
                :scissors 3
                :loss 0
                :draw 3
                :win 6})

(defn decode-round [[opp me]]
  {:moves
   [(get codebook opp)
    (get codebook me)]})

(defn resolve-round [{:keys [moves] :as round}]
  (let [[opp me] moves]
    (assoc round :result
           (cond
             (= opp me) :draw
             (= (get roshambo me) opp) :win
             :else :loss))))

(defn score-round [{:keys [moves result]}]
  (let [[opp me] moves]
    (+ (get scorebook me) (get scorebook result))))

(defn part-1 [fname]
  (let [in1 (->
             (read-input fname))]
    (->> in1
         (str/split-lines)
         (map #(str/split % #" "))
         (map decode-round)
         (map resolve-round)
         (map score-round)
         (reduce +))))

(part-1 "day02-sample.txt")
(part-1 "day02.txt")

(def codebook-new {"A" :rock
                   "B" :paper
                   "C" :scissors
                   "X" :loss
                   "Y" :draw
                   "Z" :win})

(defn decode-round-new [[opp me]]
  {:opp (get codebook-new opp)
   :result (get codebook-new me)})

(defn resolve-round-new [{:keys [opp result] :as round}]
  (assoc round :me
         (cond
           (= result :draw) opp
           (= result :win) (get roshambo-inv opp)
           :else (get roshambo opp))))

(defn score-round-new [{:keys [me result]}]
  (+ (get scorebook me) (get scorebook result)))
(defn part-2 [fname]
  (let [in1 (->
             (read-input fname))]
    (->> in1
         (str/split-lines)
         (map #(str/split % #" "))
         (map decode-round-new)
         (map resolve-round-new)
         (map score-round-new)
         (reduce +))))

(part-2 "day02-sample.txt")
(part-2 "day02.txt")
