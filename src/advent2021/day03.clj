(ns advent2021.day03
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def puzzle-input (slurp (io/resource "advent2021/day03.txt")))

(defn bit-positions [input]
  (apply map vector input))

(defn most-common [{zeros \0 ones \1 :or {zeros 0 ones 0}}]
  (if (< ones zeros) \0 \1))

(defn least-common [{zeros \0 ones \1 :or {zeros 0 ones 0}}]
  (if (<= zeros ones) \0 \1))

(defn bits->dec [& bits]
  (read-string (apply str "2r" bits)))

;; Part One
(->> puzzle-input
     str/split-lines
     bit-positions
     (map (comp (juxt most-common least-common) frequencies))
     (apply map bits->dec)
     (apply *))
;; => 4006064

;; Part Two
(defn rating [keep-bit ratings idx]
  (let [keep-bits (->> ratings bit-positions (map (comp keep-bit frequencies)))
        filtered-ratings (filter (fn [r] (= (nth r idx) (nth keep-bits idx))) ratings)]
    (if (= 1 (count filtered-ratings))
      (reduced (-> filtered-ratings first bits->dec))
      filtered-ratings)))

(defn oxygen-rating [input]
  (reduce (partial rating most-common) input (range)))

(defn co2-scrubber-rating [input]
  (reduce (partial rating least-common) input (range)))

(->> puzzle-input
     str/split-lines
     ((juxt oxygen-rating co2-scrubber-rating))
     (apply *))
;; => 5941884
