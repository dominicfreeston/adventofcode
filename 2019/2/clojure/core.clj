(ns advent.2019.2
  (:require [clojure.string :as str]))

(defn op [num]
  (case num
    1 +
    2 *
    99 nil))

(defn run-program [state]
  (loop [state state pointer 0]
    (if-let [op (op (state pointer))]
      (recur (assoc state
                    (state (+ pointer 3))
                    (op (state (state (+ pointer 1))) (state (state (+ pointer 2)))))
             (+ pointer 4))
      state)))

(defn run-risky-program [state]
  (try
    (run-program state)
    (catch Exception e [])))

(defn prepare-program [state noun verb]
  (assoc (vec state) 1 noun 2 verb))

(defn solve-1 [state]
  (first (run-program (prepare-program state 12 2))))

(defn solve-2 [state]
  (loop [noun 0 verb 0]
    (cond
      (= 19690720 (first (run-risky-program (prepare-program state noun verb))))
      (+ verb (* 100 noun))
      (> verb 99)
      (recur (inc noun) 0)
      :else
      (recur noun (inc verb)))))

(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [state (map #(Integer/parseInt %) (str/split (first *command-line-args*) #","))]
    (println (solve-1 state))
    (println (solve-2 state))))
