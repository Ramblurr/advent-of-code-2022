(ns day07
  (:require
   [clojure.walk :refer [prewalk]]
   [aoc.core :refer [read-input]]))

(defn parse [state [_ cmd path size _]]
  ;; (tap> {:state state :cmd cmd :path path :size size :fname fname})
  (if (= cmd "cd")
    (if (= path "..")
      (update state :path pop)
      (update state :path conj path))
    (update-in state
               (into [] (concat [:sizes] (:path state) [:local-size]))
               (fnil + 0)
               (parse-long size))))

(defn find-dir-with-size
  "Returns a seq of the directories with :sum matching the predicate"
  [pred acc summed]
  (reduce (fn [acc [k v]]
            (cond (= :sum k)
                  (if (pred v)
                    (conj acc v)
                    acc)
                  (map? v) (find-dir-with-size pred acc v)
                  :else acc)) acc summed))

(defn children-sum
  "Return the total size of the children"
  [total self]
  (if (int? self)
    (+ total self)
    (reduce children-sum total (vals self))))

(defn total-children
  "Given the fs, attach a :sum key to every directory indicating the sum of itself and all its children"
  [sizes]
  (prewalk (fn [v]
             (if (map? v) (assoc v :sum (children-sum 0 v))
                 v))
           (get sizes "/")))

(defn gather-fs-info [fname]
  (->> (read-input fname)
       (re-seq #"\$ (cd) (.+)|(\d+) (.+)")
       (reduce parse {:path [] :sizes {}})
       :sizes
       total-children))

(comment
  ;; Example output from gather-fs-info
  {:local-size 23352670
   :sum 48381165
   "a" {:local-size 94269
        :sum 94853
        "e" {:local-size 584
             :sum 584}}
   "d" {:local-size 24933642
        :sum 24933642}})

(defn part1 [fname]
  (->> fname
       (gather-fs-info)
       (find-dir-with-size #(<= % 100000) [])
       (reduce +)))

(defn part2 [fname]
  (let [fs (gather-fs-info fname)
        total-to-delete (+ (:sum fs) -70000000 30000000)]
    (->> fs
         (find-dir-with-size #(>= % total-to-delete) [])
         (apply min))))

(comment
  (part1 "day07-sample.txt")
  ;; => 95437
  (part1 "day07.txt")
  ;; => 1582412
  (part2 "day07-sample.txt")
  ;; => 24933642
  (part2 "day07.txt")           ;; rcf
  ;; => 3696336
  )
