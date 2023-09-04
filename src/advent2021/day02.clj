(ns advent2021.day02
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def puzzle-input (slurp (io/resource "advent2021/day02.txt")))

(defn parse [instruction]
  (let [[_ dir units] (re-matches #"(\w+)\s(\d+)" instruction)]
    [(keyword dir) (parse-long units)]))

(defn move [[pos depth] [dir units]]
  (case dir
    :forward [(+ pos units) depth]
    :down [pos (+ depth units)]
    :up [pos (- depth units)]))

;; Part One
(->> puzzle-input
     str/split-lines
     (map parse)
     (reduce move [0 0])
     (apply *))
;; => 1813801

;; Part Two
(defn move2 [[aim pos depth] [dir units]]
  (case dir
    :forward [aim (+ pos units) (+ depth (* aim units))]
    :down [(+ aim units) pos depth]
    :up [(- aim units) pos depth]))

(->> puzzle-input
     str/split-lines
     (map parse)
     (reduce move2 [0 0 0])
     ((fn [[_aim pos depth]] (* pos depth))))
;; => 1960569556
