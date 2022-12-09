(ns aoc.2022.day05
  (:require
   [aoc.core :refer [concatv peek-n pop-n pull-ints read-input third to-indexed-map transpose]]
   [clojure.string :as str]
   [medley.core :as m]))

(defn fill-in-blanks [n row]
  (concat row (repeat (max 0 (- n (count row))) [])))

(defn parse [fname]
  (let [[stack directions] (str/split (read-input fname) #"\n\n")
        stack-size (parse-long (str (last stack)))
        stack (str/split-lines stack)
        stack (subvec stack 0 (dec (count stack)))]
    {:stacks (->> stack
                  (map #(str/replace % #"    " "[]"))
                  (map #(str "[" % "]"))
                  (map read-string)
                  (map (partial fill-in-blanks stack-size))
                  transpose
                  (map #(remove empty? %))
                  (map reverse)
                  (map #(into [] %))
                  (to-indexed-map)
                  (m/map-keys inc))
     :stack-size stack-size
     :directions (->> directions
                      (str/split-lines)
                      (map pull-ints))}))

(defn exec-order [cratemover-version stacks [move-count from-n to-n]]
  (let [from-stack (get stacks from-n)
        to-stack (get stacks to-n)
        moved-items (peek-n move-count from-stack)
        _ (assert (and (some? moved-items) (seq moved-items)))
        _ (assert (= move-count (count moved-items)))]
    (-> stacks
        (assoc from-n (pop-n move-count from-stack))
        (assoc to-n (concatv to-stack (condp = cratemover-version
                                        :9001
                                        moved-items
                                        :9000
                                        (reverse moved-items)
                                        (assert false "Unhandled cratemover version")))))))

(defn execute [cratemover-version {:keys [stacks directions stack-size]}]
  {:stack-size stack-size
   :stacks (reduce (partial exec-order cratemover-version) stacks directions)
   :directions []})

(defn report [{:keys [stacks]}]
  (->> stacks
       (into (sorted-map))
       (vals)
       (map peek)
       (map first)
       (apply str)))

(defn part1 [fname]
  (->> fname
       parse
       (execute :9000)
       report))

(defn part2 [fname]
  (->> fname
       parse
       (execute :9001)
       report))

(part1 "day05-sample.txt")
(part1 "day05.txt")
;; => "WCZTHTMPS"

(part2 "day05-sample.txt")
(part2 "day05.txt")
;; => "BLSGJSDTS"

(comment
  ;; scratch
  (def stack-numbers-pat #"(?m)^\W+(\d+\W*)+$")
  (defn num-stacks [day-lines]
    (apply max
           (pull-ints
            (first
             (filter
              #(re-matches stack-numbers-pat %) day-lines)))))
  (defn simplify-slots [column]
    (map #(if (empty? %) :empty (keyword (first %))) column))

  (def _s
    (parse "day05-sample.txt"))
  (def _s1
    (exec-order (:stacks _s) (first (:directions _s))))
  (def _s2
    (exec-order _s1 (second (:directions _s))))
  (exec-order _s2 (third (:directions _s)))
  (execute _s))
