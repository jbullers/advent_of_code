(ns advent2022.day03 
  (:require [clojure.set :as set]
            [clojure.string :as str]))

(defn bisect [s] 
  (split-at (/ (count s) 2) s))

(defn shared-items [compartments]
  (apply set/intersection (map set compartments)))

(defn item-priority [item]
  (cond 
    (Character/isLowerCase item) (- (int item) 96)
    (Character/isUpperCase item) (- (int item) 38)))

(def puzzle-input (slurp "resources/advent2022/day03.txt"))

(->> puzzle-input
     str/split-lines
     (map bisect)
     (mapcat shared-items)
     (map item-priority)
     (apply +))
;; => 8252

(->> puzzle-input
     str/split-lines
     (partition 3)
     (mapcat shared-items)
     (map item-priority)
     (apply +))
;; => 2828
