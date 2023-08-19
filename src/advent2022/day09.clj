(ns advent2022.day09 
  (:require [clojure.string :as str]))

(def sample-input "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(defn motions [input]
  (let [[_ dir steps] (re-find #"([RULD]) (\d+)" input)]
    [dir steps]
    (repeat (parse-long steps) (keyword dir))))

(defn parse [input]
  (->> input
       str/split-lines
       (mapcat motions)))

(defn move [[hx hy] m]
  (case m
    :R [(inc hx) hy]
    :L [(dec hx) hy]
    :U [hx (inc hy)]
    :D [hx (dec hy)]))

(defn adjacent? [tx ty hx hy]
  (and (<= (abs (- tx hx)) 1)
       (<= (abs (- ty hy)) 1)))

(defn adjust-horizontal [tx ty hx]
  (if (< tx hx)
    [(inc tx) ty]
    [(dec tx) ty]))

(defn adjust-vertical [tx ty hy]
  (if (< ty hy)
    [tx (inc ty)]
    [tx (dec ty)]))

(defn adjust-diagonal [tx ty hx hy]
  (cond
    (and (< tx hx) (< ty hy)) [(inc tx) (inc ty)]
    (and (< hx tx) (< ty hy)) [(dec tx) (inc ty)]
    (and (< tx hx) (< hy ty)) [(inc tx) (dec ty)]
    (and (< hx tx) (< hy ty)) [(dec tx) (dec ty)]))

(defn adjust [[tx ty] [hx hy]]
  (cond
    (adjacent? tx ty hx hy) [tx ty]
    (= ty hy) (adjust-horizontal tx ty hx)
    (= tx hx) (adjust-vertical tx ty hy)
    :else (adjust-diagonal tx ty hx hy)))

(defn simulate [[head-positions & tails] motion]
  (let [h (last head-positions)
        h' (move h motion)]
    (loop [prev h'
           positions tails
           updated-positions [(conj head-positions h')]]
      (if (seq positions)
        (let [tail-positions (first positions)
              t (last tail-positions)
              t' (adjust t prev)]
          (recur t' (rest positions) (conj updated-positions (conj tail-positions t'))))
        updated-positions))))

(def puzzle-input (slurp "resources/advent2022/day09.txt"))

(->> puzzle-input
     parse
     (reduce simulate [[[0 0]] [[0 0]]])
     last
     distinct
     count)
;; => 6212

(->> puzzle-input
     parse
     (reduce simulate (repeat 10 [[0 0]]))
     last
     distinct
     count)
;; => 2522

