(ns advent2022.day02
  (:require [clojure.string :as str]))

(defn decode [encryption round]
  [(encryption (first round)) (encryption (last round))])

(def selected-score {:rock 1 :paper 2 :scissors 3})

(def round-score {:win 6 :draw 3 :lose 0})

(defn determine-winner [opponent me]
  (condp = [opponent me]
    [:rock :rock] :draw
    [:rock :paper] :win
    [:rock :scissors] :lose

    [:paper :rock] :lose
    [:paper :paper] :draw
    [:paper :scissors] :win

    [:scissors :rock] :win
    [:scissors :paper] :lose
    [:scissors :scissors] :draw))

(defn score-round [[opponent me]]
  (+ (round-score (determine-winner opponent me))
     (selected-score me)))

(def puzzle-input (slurp "resources/advent2022/day02.txt"))

(->> puzzle-input
     str/split-lines
     (map (partial decode {\A :rock \X :rock
                           \B :paper \Y :paper
                           \C :scissors \Z :scissors}))
     (map score-round)
     (apply +))
;; => 13484

(defn determine-move [[opponent outcome]]
  (condp = [opponent outcome]
    [:rock :lose] [:rock :scissors]
    [:rock :draw] [:rock :rock]
    [:rock :win] [:rock :paper]

    [:paper :lose] [:paper :rock]
    [:paper :draw] [:paper :paper]
    [:paper :win] [:paper :scissors]

    [:scissors :lose] [:scissors :paper]
    [:scissors :draw] [:scissors :scissors]
    [:scissors :win] [:scissors :rock]))

(->> puzzle-input
     str/split-lines
     (map (partial decode {\A :rock \X :lose
                           \B :paper \Y :draw
                           \C :scissors \Z :win}))
     (map determine-move)
     (map score-round)
     (apply +))
;; => 13433
