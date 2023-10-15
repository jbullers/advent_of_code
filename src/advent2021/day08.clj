(ns advent2021.day08
  (:require
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]))

(def sample-input "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce")

(defn parse-entry [entry]
  (let [[patterns value] (str/split entry #"\s\|\s")]
    [(map set (str/split patterns #" ")) (map set (str/split value #" "))]))

(defn parse [input]
  (map parse-entry (str/split-lines input)))

;; Part 1
(->> (parse sample-input)
     (mapcat second)
     (map count)
     (filter #{2 4 3 7})
     (count))
;; => 26

(def puzzle-input (slurp (io/resource "advent2021/day08.txt")))

(->> (parse puzzle-input)
     (mapcat second)
     (map count)
     (filter #{2 4 3 7})
     (count))
;; => 421

;; Part 2

(defn common-segments [pattern1 pattern2]
  (count (set/intersection pattern1 pattern2)))

;; 2 -> 5 segments, 2/5 in common with 7, 2/5 in common with 4
;; 5 -> 5 segments, 2/5 in common with 7, 3/5 in common with 4
;; 3 -> 5 segments, 3/5 in common with 7, 3/5 in common with 4
(defn decode-5-segments [known-patterns signal-pattern]
  (if (= (count signal-pattern) 5)
    (if (= (common-segments (known-patterns 7) signal-pattern) 3)
      (assoc known-patterns 3 signal-pattern)
      (if (= (common-segments (known-patterns 4) signal-pattern) 2)
        (assoc known-patterns 2 signal-pattern)
        (assoc known-patterns 5 signal-pattern)))
    known-patterns))

;; 6 -> 6 segments, 2/6 in common with 7, 3/6 in common with 4
;; 9 -> 6 segments, 3/6 in common with 7, 4/6 in common with 4
;; 0 -> 6 segments, 3/6 in common with 7, 3/6 in common with 4
(defn decode-6-segments [known-patterns signal-pattern]
  (if (= (count signal-pattern) 6)
    (if (= (common-segments (known-patterns 7) signal-pattern) 2)
      (assoc known-patterns 6 signal-pattern)
      (if (= (common-segments (known-patterns 4) signal-pattern) 4)
        (assoc known-patterns 9 signal-pattern)
        (assoc known-patterns 0 signal-pattern)))
    known-patterns))

(defn decode-unique [known-patterns signal-pattern]
  (case (count signal-pattern)
    2 (assoc known-patterns 1 signal-pattern)
    3 (assoc known-patterns 7 signal-pattern)
    4 (assoc known-patterns 4 signal-pattern)
    7 (assoc known-patterns 8 signal-pattern)
    known-patterns))

(defn decode [[signal-patterns output-value]]
  (let [unique-decoded (reduce decode-unique {} signal-patterns)
        unique+5-seg-decoded (reduce decode-5-segments unique-decoded signal-patterns)
        fully-decoded (reduce decode-6-segments unique+5-seg-decoded signal-patterns)]
    [fully-decoded output-value]))

(defn output-value [[signal-patterns output-value]]
  (let [pattern->digit (into {} (map (juxt val key)) signal-patterns)
        [a b c d] (map pattern->digit output-value)]
    (+ (* a 1000) (* b 100) (* c 10) d)))

(->> puzzle-input
     (parse)
     (map decode)
     (map output-value)
     (reduce +))
;; => 986163
