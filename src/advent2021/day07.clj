(ns advent2021.day07
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn median [nums]
  (let [sorted-nums (sort nums)
        count-nums (count sorted-nums)
        middle (/ count-nums 2)]
    (if (even? count-nums)
      (/ (+ (nth sorted-nums (dec middle))
            (nth sorted-nums middle))
         2)
      (nth sorted-nums middle))))

(defn crab-distance [target crab]
  (abs (- target crab)))

(defn parse [input]
  (map parse-long (str/split input #",")))

(def puzzle-input (slurp (io/resource "advent2021/day07.txt")))

;; Part 1
(let [crabs (parse puzzle-input)
      median (median crabs)]
  (->> crabs
       (map #(crab-distance median %))
       (reduce +)))
;; => 347011

;; Part 2

(defn mean [nums]
  (/ (apply + nums) (count nums)))

(defn sum [n]
  (/ (* n (+ n 1)) 2))

(let [crabs (parse puzzle-input)
      mean (int (mean crabs))]
  (->> crabs
       (map #((comp sum crab-distance) mean %))
       (reduce +)))
;; => 98363777
