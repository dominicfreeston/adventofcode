(ns advent.2019.6
  (:require [clojure.string :as str]
            [clojure.set :as s]))

(defn parse-orbiters [state orbit]
  (let [[orbitee orbiter] (str/split orbit #"\)")]
    (assoc state orbiter orbitee)))

(defn orbitees [state orbiter]
  (loop [orbiter orbiter orbitees []]
    (if-let [next-orbiter (state orbiter)]
      (recur next-orbiter (conj orbitees next-orbiter))
      orbitees)))

(defn count-orbitees [state orbiter]
  (count (orbitees state orbiter)))

(defn solve-1 [state]
  (reduce + (map (partial count-orbitees state) (keys state))))

(defn solve-2 [state]
  (let [san-orbitees (set (orbitees state "SAN"))
        you-orbitees (set (orbitees state "YOU"))
        diff (s/difference san-orbitees you-orbitees)]
    (count (s/union (s/difference san-orbitees you-orbitees)
                    (s/difference you-orbitees san-orbitees)))))

(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [state (reduce parse-orbiters {} *command-line-args*)]
    (println (solve-1 state))
    (println (solve-2 state))))
