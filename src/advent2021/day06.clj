(ns advent2021.day06
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def reset-fish 6)
(def spawned-fish 8)

(defn fish-timer [state fish]
  (let [updated-fish (dec fish)]
    (if (neg? updated-fish)
      (conj state reset-fish spawned-fish)
      (conj state updated-fish))))

(defn simulate [n step-fn initial-state]
  (nth (iterate step-fn initial-state) n))

(defn parse [input]
  (map parse-long (str/split input #",")))

(def puzzle-input (slurp (io/resource "advent2021/day06.txt")))

(->> (parse puzzle-input)
     (simulate 80 #(reduce fish-timer [] %))
     (count))
;; => 380758

;; Keeping the above instead of editing it directly. It might be interesting
;; to look back on as a reference.
;;
;; Unsurprisingly, this runs for a really, really long time
;; (the puzzle did note exponential growth).
;; (-> (iterate simulate (parse puzzle-input))
;;     (nth 256)
;;     (count))
;;
;; What we need is an algorithm that grows slower, or perhaps not at all.
;; I'm thinking the note that we count starting from 0 is a bit of a hint:
;; an array of elements from 0 to 8 would speed things up significantly.
;; Since we only need to count the total number of lanternfish, and the
;; fish all update every tick, it should just be a matter of tracking how
;; many fish are at each "internal timer" value, and shifting down each tick,
;; adding/replacing the ticked 0 fish.

(defn tick-fish-timer [[num-zero-fish & remaining]]
  (-> (vec remaining)
      (conj num-zero-fish)
      (update reset-fish + num-zero-fish)))

(defn bucket-fish-by-timer [fish]
  (let [buckets (vec (repeat 9 0))]
    (reduce (fn [b f] (update b f inc)) buckets fish)))

(->> (parse puzzle-input)
     (bucket-fish-by-timer)
     (simulate 256 tick-fish-timer)
     (reduce +))
;; => 1710623015163
