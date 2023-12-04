(ns user
  (:require
   [portal.api :as p]
   [nextjournal.clerk :as clerk]))

(defn go []

  (p/open {:theme :portal.colors/gruvbox :port  5678})
  (add-tap p/submit)
  (tap> 1))

(defn clerk []
  (clerk/serve! {:browse? true}))

(comment

  ;; start Clerk's built-in webserver on the default port 7777, opening the browser when done
  (clerk/halt!)

  (clerk/show! "src/aoc/2022/day01.clj")

  (go)
  ;;
  )
