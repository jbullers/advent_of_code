(ns advent2022.day06)

(def sample-input "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")

(def packet 4)
(def message 14)

(defn find-start-of-signal [n signal]
  (let [packets (partition n 1 signal)
        start (->> packets
                   (keep-indexed #(when (apply distinct? %2) %1))
                   first)]
    (+ n start)))

(find-start-of-signal packet sample-input)
;; => 10

(find-start-of-signal message sample-input)
;; => 29

(def puzzle-input (slurp "resources/advent2022/day06.txt"))

(find-start-of-signal packet puzzle-input)
;; => 1531

(find-start-of-signal message puzzle-input)
;; => 2518
