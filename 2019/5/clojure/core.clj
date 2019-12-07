(ns advent.2019.5
  (:require [clojure.string :as str]))

(defn readval
  "Reads a parameter value from memory using the appropriate mode"
  [{:keys [memory pointer modes]} param]
  (let [mode (get modes (dec param) 0)]
   (case mode
     0 (memory (memory (+ pointer param)))
     1 (memory (+ pointer param)))))

;; Operations

(defn add-op [{:keys [memory pointer] :as state}]
  (assoc state
         :memory (assoc memory
                        (memory (+ pointer 3))
                        (+ (readval state 1) (readval state 2)))
         :pointer (+ pointer 4)))

(defn mult-op [{:keys [memory pointer] :as state}]
  (assoc state
         :memory (assoc memory
                        (memory (+ pointer 3))
                        (* (readval state 1) (readval state 2)))
         :pointer (+ pointer 4)))

(defn input-op [{:keys [memory input pointer] :as state}]
  (assoc state
         :memory (assoc memory
                        (memory (inc pointer))
                        input)
         :pointer (+ pointer 2)))

(defn output-op [{:keys [output memory pointer] :as state}]
  (assoc state
         :output (conj output (readval state 1))
         :pointer (+ pointer 2)))

(defn jump-true-op [state]
  (if (zero? (readval state 1))
    (update state :pointer (partial + 3))
    (assoc state :pointer (readval state 2))))

(defn jump-false-op [state]
  (if (zero? (readval state 1))
    (assoc state :pointer (readval state 2))
    (update state :pointer (partial + 3))))

(defn less-than-op [{:keys [memory pointer] :as state}]
  (assoc state
         :memory (assoc memory
                        (memory (+ pointer 3))
                        (if (< (readval state 1) (readval state 2)) 1 0))
         :pointer (+ pointer 4)))

(defn equals-op [{:keys [memory pointer] :as state}]
  (assoc state
         :memory (assoc memory
                        (memory (+ pointer 3))
                        (if (= (readval state 1) (readval state 2)) 1 0))
         :pointer (+ pointer 4)))

;; Operation parsing

(defn op-def
  "Returns the matching function and the number of parameters"
  [num]
  (case num
    1 add-op
    2 mult-op
    3 input-op
    4 output-op
    5 jump-true-op
    6 jump-false-op
    7 less-than-op
    8 equals-op
    99 nil))

(defn parse-op
  [num]
  (if-let [op (op-def (rem num 100))]
    (loop [modes [], n (quot num 100)]
      (if (pos? n)
        (recur (conj modes (rem n 10))
               (quot n 10))
        [op modes]))
    nil))

;; Main run loop

(defn run-program
  [state input]
  (loop [{:keys [memory pointer] :as state} {:memory (vec state) :pointer 0 :input input :output []}]
    (if-let [[op modes] (parse-op (memory pointer))]
      (recur (op (assoc state :modes modes)))
      (:output state))))

(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [state (map #(Integer/parseInt %) (str/split (first *command-line-args*) #","))]
    (println (last (run-program state 1)))
    (println (last (run-program state 5)))))
