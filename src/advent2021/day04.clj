(ns advent2021.day04
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn cell [n]
  [(parse-long n) false])

(def cell-num first)
(def called? last)

(defn parse-calls [input]
  (map parse-long (str/split input #",")))

(defn parse-board [input]
  (map cell (str/split input #"\s+")))

(defn winner? [board]
  (let [rows (partition 5 board)
        cols (apply map vector rows)
        every-called? (fn [cells] (when (every? called? cells) board))]
    (or (some every-called? rows)
        (some every-called? cols))))

(defn mark-called [n board]
  (map (fn [cell]
         (if (= (cell-num cell) n)
           [n true]
           cell))
       board))

(defn play [calls boards]
  (reduce (fn [boards n]
            (let [updated-boards (map (partial mark-called n) boards)]
              (if-let [winning-board (some winner? updated-boards)]
                (reduced (vector n winning-board))
                updated-boards)))
          boards
          calls))

(defn score [[winning-call winning-board]]
  (->> winning-board
       (remove called?)
       (map cell-num)
       (reduce + 0)
       (* winning-call)))

(def puzzle-input (slurp (io/resource "advent2021/day04.txt")))

;; Part One
(let [[calls & boards] (str/split puzzle-input #"\n\n\s*")]
  (score (play (parse-calls calls) (map parse-board boards))))
;; => 64084

;; Part Two
(defn play2 [calls boards]
  (reduce (fn [boards n]
            (let [updated-boards (map (partial mark-called n) boards)
                  remaining-boards (remove winner? updated-boards)]
              (if (empty? remaining-boards)
                (reduced (vector n (last updated-boards)))
                remaining-boards)))
          boards
          calls))

(let [[calls & boards] (str/split puzzle-input #"\n\n\s*")]
  (score (play2 (parse-calls calls) (map parse-board boards))))
;; => 12833
