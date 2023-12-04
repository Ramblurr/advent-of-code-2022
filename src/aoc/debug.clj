(ns aoc.debug)

(defn tap>> [v]
  (tap> v)
  v)

(defn xxx [msg v]
  (tap> {:msg msg :v v})
  v)

(defn xxx-mark [msg v]
  (tap> {:msg msg})
  v)
