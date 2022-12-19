(ns advent.2022.2
  (:require [clojure.string :as str]))

(comment
  ;; evaluate this if running the code below from a repl started from this file
  

  (parse-round-wrong "A X")
  (parse-round-right "A X")
  
  (score-round [:paper :scissors])
  ,)


(defn decrypt-move [move]
  (case move
    (\A \X) :rock
    (\B \Y) :paper
    (\C \Z) :scissors))

(defn parse-round-wrong [round]
  (mapcat
   #(map decrypt-move %)
   (str/split round #"\W")))

(def results
  [[:rock :paper]
   [:paper :scissors]
   [:scissors :rock]])

(defn decrypt-round [move result]
  [move (case result
          \X (some (fn [[l w]] (when (= w move) l)) results)
          \Y move
          \Z (some (fn [[l w]] (when (= l move) w)) results))])

(defn parse-round-right [round]
  (let [[[m] [r]] (str/split round #"\W")]
    (decrypt-round (decrypt-move m) r)))

(defn score-round [round]
  (+
   (case (second round)
     :rock 1
     :paper 2
     :scissors 3)
   (cond
      (apply = round) 3
      (some #{round} results) 6
      :else 0)))


(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [input (slurp (first *command-line-args*))
        rounds (str/split input #"\n")]
    (println (reduce + (map (comp score-round parse-round-wrong) rounds)))
    (println (reduce + (map (comp score-round parse-round-right) rounds)))))

