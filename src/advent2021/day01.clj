(ns advent2021.day01
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def puzzle-input (slurp (io/resource "advent2021/day01.txt")))

(defn parse [input]
  (->> input
       (str/split-lines)
       (map parse-long)))

(defn count-increases [nums]
  (->> nums
       (partition 2 1)
       (map (fn [[a b]] (- b a)))
       (filter pos?)
       count))

;; Part One
(->> puzzle-input
     parse
     count-increases)
;; => 1451

;; Part Two
(->> puzzle-input
     parse
     (partition 3 1)
     (map #(apply + %))
     count-increases)
;; => 1395
