(ns advent2022.day01 
  (:require [clojure.string :as str]))

(def puzzle-input (slurp "resources/advent2022/day01.txt"))

(defn total-calories [calories]
  (transduce (map parse-long) + (str/split-lines calories)))

(->> (str/split puzzle-input #"\n\n")
     (map total-calories)
     (apply max))
;; => 74711

(->> (str/split puzzle-input #"\n\n")
     (map total-calories)
     (sort >)
     (take 3)
     (apply +))
;; => 209481
