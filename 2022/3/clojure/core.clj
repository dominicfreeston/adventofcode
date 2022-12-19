(ns advent.2022.3
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(comment
  (def *command-line-args* ["../input.txt"])
  (find-wrong-item "abcdea")

  (item-priority \Z)
  (find-badge ["abcd" "bcde" "defg"])
  )

(defn item-priority [i]
  (if (<= (int i) 90)
    (- (int i) 38)
    (- (int i) 96)))

(defn find-wrong-item [s]
  (let [c (/ (count s) 2)
        a (set (take c s))
        b (set (drop c s))]
    (first (set/intersection a b))))

(defn find-badge [bags]
  (first (apply set/intersection (map set bags))))

(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [input (slurp (first *command-line-args*))
        bags (str/split input #"\n")]
    (println (reduce + (map (comp item-priority find-wrong-item) bags)))
    (println (reduce + (map (comp item-priority find-badge) (partition 3 bags))))))
