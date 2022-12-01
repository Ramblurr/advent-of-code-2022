(ns user
  (:require [nextjournal.clerk :as clerk]))

(comment

;; start Clerk's built-in webserver on the default port 7777, opening the browser when done
  (clerk/serve! {:browse? true})
  (clerk/halt!)

  (clerk/show! "src/aoc/day01.clj")
  ;;
  )
