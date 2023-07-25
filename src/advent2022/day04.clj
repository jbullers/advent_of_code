(ns advent2022.day04 
  (:require [clojure.string :as str]))

(defn section-assignments [pair]
  (->> pair
       (re-find #"(\d+)-(\d+),(\d+)-(\d+)")
       rest
       (map parse-long)))

(defn fully-contained? [[min1 max1 min2 max2]]
  (or (<= min1 min2 max2 max1)
      (<= min2 min1 max1 max2)))

(def puzzle-input (slurp "resources/advent2022/day04.txt"))

(->> puzzle-input
     str/split-lines
     (map section-assignments)
     (filter fully-contained?)
     count)
;; => 644


(defn overlap? [[min1 max1 min2 max2]]
  (or
   (<= min1 min2 max1 max2)
   (<= min2 min1 max2 max1)))

(->> puzzle-input
     str/split-lines
     (map section-assignments)
     (filter (some-fn overlap? fully-contained?))
     count)
;; => 926
