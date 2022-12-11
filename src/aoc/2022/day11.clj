(ns aoc.2022.day11
  (:require
   [clojure.string :as str]
   [aoc.core :as core :refer [read-input read-input-lines]]
   [medley.core :as m]))

(def sample (read-input "2022/day11-sample.txt"))
(def input (read-input "2022/day11.txt"))

(defn parse-block [idx group]
  (let [[_ items operation test if-true if-false] (str/split group #"\n")]
    {:monkey idx
     :items (into [] (core/pull-ints items))
     :operation-arg (or (first (core/pull-ints operation)) :old)
     :operation-op (if (str/includes? operation "*") * +)
     :divisible-by (first (core/pull-ints test))
     :inspect-count 0
     :if-true-monkey (first (core/pull-ints if-true))
     :if-false-monkey (first (core/pull-ints if-false))}))

(defn parse [input]
  (->> (str/split input #"\n\n")
       (map-indexed parse-block)
       (into [])))

(defn monkey-turn [rules state monkey-idx]
  (assert (vector? (get-in state [monkey-idx :items])))
  (let [{:keys [if-true-monkey if-false-monkey items operation-arg operation-op divisible-by] :as monkey} (get state monkey-idx)
        inspected-items (->> items
                             (map #(operation-op % (if (= :old operation-arg) % operation-arg)))
                             (map (get rules :worry-fn)))
        throw-items (->> inspected-items
                         (map
                          #(if (= (mod % divisible-by) 0)
                             if-true-monkey
                             if-false-monkey)))]
    (reduce (fn [state [item receiver]]
              (update-in state [receiver :items] conj item))
            (-> state
                (assoc-in [monkey-idx :items] [])
                (update-in [monkey-idx :inspect-count] + (count inspected-items)))
            (map vector inspected-items throw-items))))

(defn round [rules state round-n]
  (reduce (partial  monkey-turn rules) state (range (count state))))

(defn monkey-business [state]
  (->> state
       (map :inspect-count)
       (sort >)
       (take 2)
       (reduce *)))

(defn part-1 [input]
  (let [rules {:worry-fn (fn [item] (quot item 3))}]
    (->> (range 20)
         (reduce (partial round rules) (parse input))
         (monkey-business))))

(defn part-2 [input]
  (let [state (parse input)
        lcm (->> state (map :divisible-by) (reduce *))
        rules {:worry-fn (fn [item] (mod item lcm))}]
    (->> (range 10000)
         (reduce (partial round rules) state)
         (monkey-business))))
(comment
  (part-1 input)
  ;; =>  118674
  (part-2 input)
  ;; => 32333418600
  )
