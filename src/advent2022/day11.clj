(ns advent2022.day11 
  (:require [clojure.string :as str]))

(defn parse [line]
  (condp re-matches line
    #"Monkey (\d):"
    :>> (fn [[_ id]]
          [:id (parse-long id)])

    #"\s*Starting items: (.+)"
    :>> (fn [[_ items]]
          [:items (mapv parse-long (str/split items #", "))])

    #"\s*Operation: new = old ([/\+\-\*]) (\d+|old)"
    :>> (fn [[_ op term]]
          [:op [(-> (str op "'") symbol resolve)
                (if (= term "old") :old (parse-long term))]])

    #"\s*Test: divisible by (\d+)"
    :>> (fn [[_ n]]
          [:test (parse-long n)])

    #"\s*If true: throw to monkey (\d)"
    :>> (fn [[_ n]]
          [:true-monkey (parse-long n)])

    #"\s*If false: throw to monkey (\d)"
    :>> (fn [[_ n]]
          [:false-monkey (parse-long n)])))

(defn monkey [input]
  (->> input
       str/split-lines
       (map parse)
       (into {:inspected-count 0})))

(def sample-input "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(defn monkey-turn [worry-reducer monkeys {[op n] :op :keys [id items test true-monkey false-monkey]}]
  (loop [[item & items] items
         updated-monkeys monkeys]
    (if item
      (let [inspected-item (worry-reducer (op item (if (= :old n) item n)))
            target-monkey (if (zero? (mod inspected-item test)) true-monkey false-monkey)]
        (recur items (-> updated-monkeys
                         (update-in [id :inspected-count] inc)
                         (assoc-in [id :items] (vec items))
                         (update-in [target-monkey :items] conj inspected-item))))
      updated-monkeys)))

(defn turn [{:keys [monkeys worry-reducer] :as state}]
  (loop [idx 0
         updated-monkeys monkeys]
    (if-let [curr (get updated-monkeys idx)]
      (recur (inc idx) (monkey-turn worry-reducer updated-monkeys curr))
      (assoc state :monkeys updated-monkeys))))

(defn monkey-business [turns state]
  (let [{:keys [monkeys]} (-> (iterate turn state) (nth turns))
        sorted-inspections (->> monkeys (map :inspected-count) (sort >))]
    (->> sorted-inspections (take 2) (apply *))))

(let [monkeys (->> (str/split sample-input #"\n\n")
                   (mapv monkey))]
  (monkey-business 20 {:monkeys monkeys
                       :worry-reducer (fn [worry-level] (quot worry-level 3))}))
;; => 10605

(def puzzle-input (slurp "resources/advent2022/day11.txt"))

(let [monkeys (->> (str/split puzzle-input #"\n\n")
                   (mapv monkey))]
     (monkey-business 20 {:monkeys monkeys
                          :worry-reducer (fn [worry-level] (quot worry-level 3))}))
;; => 117640

(let [monkeys (->> (str/split puzzle-input #"\n\n") (mapv monkey))
      divisor (->> monkeys (map :test) (apply *))]
  (monkey-business 10000 {:monkeys monkeys
                          :worry-reducer (fn [worry-level] (mod worry-level divisor))}))
;; => 30616425600
