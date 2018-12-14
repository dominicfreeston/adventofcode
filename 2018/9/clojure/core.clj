(ns advent.2018.9
  (:require [clojure.zip :as zip]))

(def p-count 431)

(def start-state
  {:player 1
   :marble 1
   :m-count 70950
   :scores (zipmap (range (inc p-count)) (repeat 0))
   :circle (zip/down (zip/seq-zip '(0)))})

(defn place-marble [{:keys [marble circle] :as state}]
  (let [next-loc (or (zip/right circle) (zip/leftmost circle))]
    (assoc state :circle (zip/right (zip/insert-right next-loc marble)))))

(defn score-time [state]
  (loop [{:keys [marble circle] :as state} state moves 7]
    (if (zero? moves)
      (let [score (zip/node circle)
            rloc (zip/remove circle)]
        [(assoc state :circle (or (zip/right rloc) (zip/leftmost rloc))) score])
      (let [next-loc (or (zip/left circle) (zip/rightmost circle))]
        (recur (assoc state :circle next-loc) (dec moves))))))

(defn next-player [p]
  (if (>= p p-count) 1 (inc p)))

(defn take-turn [{:keys [player marble] :as state}]
  (if (zero? (mod marble 23))
    (let [[state score] (score-time state)]
      (update-in state [:scores player] + marble score))
    (place-marble state)))

(defn play-game [state]
  (loop [{:keys [player marble m-count] :as state} state]
    (if (> marble m-count)
      state
      (recur (assoc (take-turn state)
                    :player (next-player player)
                    :marble (inc marble))))))

(defn circle [{:keys [circle]}]
  (zip/node (zip/up circle)))

(defn top-score [state]
  (apply max (vals (state :scores))))

(println (top-score (play-game start-state)))
;; This may take a little while...
(println (top-score (play-game (update-in start-state [:m-count] * 100))))
