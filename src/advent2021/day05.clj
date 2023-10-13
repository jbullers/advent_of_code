(ns advent2021.day05
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(defn normalize [x]
  (cond
    (zero? x) 0
    (pos? x) 1
    :else -1))

(defn slope [[x1 y1] [x2 y2]]
  (let [rise (normalize (- y2 y1))
        run (normalize(- x2 x1))]
    [rise run]))

(defn covered-points [[start end]]
  (let [[rise run] (slope start end)
        next-point (fn [[x y]] [(+ x run) (+ y rise)])
        not-end? (fn [p] (not= p end))]
    (list* end (take-while not-end? (iterate next-point start)))))

(defn horizontal? [[[x1 y1] [x2 y2]]]
  (= y1 y2))

(defn vertical? [[[x1 y1] [x2 y2]]]
  (= x1 x2))

(def sample-input "0,9 -> 5,9
8,0 -> 0,8
9,4 -> 3,4
2,2 -> 2,1
7,0 -> 7,4
6,4 -> 2,0
0,9 -> 2,9
3,4 -> 1,4
0,0 -> 8,8
5,5 -> 8,2")

(defn parse-line [input]
  (let [[_ x1 y1 x2 y2] (re-matches #"(\d+),(\d+) -> (\d+),(\d+)" input)]
    [[(parse-long x1) (parse-long y1)] [(parse-long x2) (parse-long y2)]]))

(defn parse [input]
  (map parse-line (str/split-lines input)))

(def puzzle-input (slurp (io/resource "advent2021/day05.txt")))

;; Part 1
(->> (parse puzzle-input)
     (filter (some-fn horizontal? vertical?))
     (mapcat covered-points)
     (frequencies)
     (filter (fn [[_ covered-count]] (< 1 covered-count)))
     (count))
;; => 6710

;; Part 2
(->> (parse puzzle-input)
     (mapcat covered-points)
     (frequencies)
     (filter (fn [[_ covered-count]] (< 1 covered-count)))
     (count))
;; => 20121
