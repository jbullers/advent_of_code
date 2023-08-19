(ns advent2022.day10 
  (:require [clojure.string :as str]))

(defn instructions [input]
  (condp re-matches input
    #"noop" [:noop]
    #"addx (-?\d+)" :>> (fn [[_ n]] [:noop (parse-long n)])))

(defn parse [input]
  (->> input
       str/split-lines
       (mapcat instructions)))

(defn execute [[cycle x] instr]
  (if (= instr :noop)
    [(inc cycle) x]
    [(inc cycle) (+ x instr)]))

(defn signal-strengths [instructions]
  (let [cycles (reductions execute [1 1] instructions)
        cycles (take 6 (take-nth 40 (drop 19 cycles)))]
    (map #(apply * %) cycles)))

(def puzzle-input (slurp "resources/advent2022/day10.txt"))

(->> (parse puzzle-input)
     signal-strengths
     (apply +))
;; => 15880

(defn draw [screen [cycle x]]
  (if (#{x (inc x) (+ x 2)} (mod cycle 40))
    (assoc screen (dec cycle) "#")
    screen))

(->> (parse puzzle-input)
     (reductions execute [1 1])
     (reduce draw (vec (repeat 240 ".")))
     (partition 40)
     (map #(apply str %)))
;; => ("###..#.....##..####.#..#..##..####..##.#"
;;     "#..#.#....#..#.#....#.#..#..#....#.#..##"
;;     "#..#.#....#....###..##...#..#...#..#...#"
;;     "###..#....#.##.#....#.#..####..#...#.##."
;;     "#....#....#..#.#....#.#..#..#.#....#..#."
;;     "#....####..###.#....#..#.#..#.####..###.")

