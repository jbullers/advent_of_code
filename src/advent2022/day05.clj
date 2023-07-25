(ns advent2022.day05
  (:require [clojure.string :as str]))

(def sample-input "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defn parse-crates [crates]
  (->> crates
       str/split-lines
       (apply map str)
       (map str/trim)
       (filter #(re-matches #"\w+" %))
       (map butlast)
       vec))

(defn parse-move [move]
  (let [[_ col from to] (re-find #"move (\d+) from (\d+) to (\d+)" move)]
    {:move (parse-long col) :from (parse-long from) :to (parse-long to)}))

(defn parse-input [input]
  (let [[crates moves] (str/split input #"\n\n")
        crates (parse-crates crates)
        moves (map parse-move (str/split-lines moves))]
    [crates moves]))

(defn apply-move [crates move]
  (if (pos? (:move move))
    (let [from-col (dec (:from move))
          to-col (dec (:to move))
          [crate & remaining] (crates from-col)]
      (apply-move
       (-> crates
           (assoc from-col remaining)
           (assoc to-col (cons crate (crates to-col))))
       (update move :move dec)))
    crates))

(defn move-crates [mover input]
  (let [[crates moves] input]
    (reduce mover crates moves)))

(->> (parse-input sample-input)
     (move-crates apply-move)
     (map first)
     (apply str))
;; => "CMZ"

(def puzzle-input (slurp "resources/advent2022/day05.txt"))

(->> (parse-input puzzle-input)
     (move-crates apply-move)
     (map first)
     (apply str))
;; => "LBLVVTVLP"



(defn apply-move2 [crates move]
  (let [from-col (dec (:from move))
        to-col (dec (:to move))
        [crates-to-move remaining] (split-at (:move move) (crates from-col))]
    (-> crates
        (assoc from-col remaining)
        (assoc to-col (concat crates-to-move (crates to-col))))))

(->> (parse-input sample-input)
     (move-crates apply-move2)
     (map first)
     (apply str))
;; => "MCD"

(->> (parse-input puzzle-input)
     (move-crates apply-move2)
     (map first)
     (apply str))
;; => "TPFFBDRJD"

