(ns advent2022.day08
  (:require [clojure.string :as str]))

(def sample-input "30373
25512
65332
33549
35390")

(defn parse [input]
  (letfn [(parse-int [c] (Character/digit c 10))]
    (->> (str/split-lines input)
         (map #(map parse-int %)))))

(defn visible? [n trees]
  (let [[prev [curr & nxt]] (split-at n trees)]
    (or (every? #(< % curr) prev)
        (every? #(< % curr) nxt))))

(defn interior-visible [tree-heights]
  (let [rows tree-heights
        cols (apply map vector tree-heights)
        visibility (for [r (range 1 (dec (count rows)))
                         c (range 1 (dec (count cols)))]
                     (or (visible? c (nth rows r)) (visible? r (nth cols c))))]
    (->> visibility (filter true?) count)))

(defn exterior-visible [tree-heights]
  (let [rows (count tree-heights)
        cols (count (first tree-heights))]
    (- (+ (* 2 rows) (* 2 cols)) 4)))

(defn total-visible [tree-heights]
  (+ (interior-visible tree-heights) (exterior-visible tree-heights)))

(def puzzle-input (slurp "resources/advent2022/day08.txt"))

(total-visible (parse puzzle-input))
;; => 1679

(defn view [n tree-heights]
  (let [[prev [curr & nxt]] (split-at n tree-heights)
        score (fn [n height]
                (if (< height curr)
                  (inc n)
                  (reduced (inc n))))]
    (* (reduce score 0 (reverse prev))
       (reduce score 0 nxt))))

(defn scenic-scores [tree-heights]
  (let [rows tree-heights
        cols (apply map vector tree-heights)]
    (for [r (range 1 (dec (count rows)))
          c (range 1 (dec (count cols)))]
      (* (view c (nth rows r))
         (view r (nth cols c))))))

(->> puzzle-input
     parse
     scenic-scores
     (apply max))
;; => 536625
