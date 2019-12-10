(ns advent.2019.7
  (:require [clojure.string :as str]))

;; Intcode Computer From Day 5
;; with these change:
;; * takes an array of inputs and consumes them
;; * suspends execution and returns a single output
;; * :terminated is now a state

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
                        (first input))
         :pointer (+ pointer 2)
         :input (rest input)))

(defn output-op [{:keys [output memory pointer] :as state}]
  (assoc state
         :output (readval state 1)
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

(defn terminate-op [state]
  (assoc state
         :terminated true))

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
    99 terminate-op))

(defn parse-op
  [num]
  (if-let [op (op-def (rem num 100))]
    (loop [modes [], n (quot num 100)]
      (if (pos? n)
        (recur (conj modes (rem n 10))
               (quot n 10))
        [op modes]))
    nil))

;; Day 7 code starts here
;;;;;;;;;;;;;;;;;;;;;;;;;

;; Main run loop
;; Now returns the whole state so program can be queried and resumed
;; Returns whether it's terminated or whether it outputs something

(defn prepare-program
  [state input]
  {:memory (vec state)
   :pointer 0
   :input input
   :output nil})

(defn run-program
  [program input]
  (loop [{:keys [memory pointer] :as state} (assoc program
                                                   :input (concat (:input program) input)
                                                   :output nil)]
    (let [[op modes] (parse-op (memory pointer))
          state (op (assoc state :modes modes))]
      (if (or (:terminated state) (some? (:output state)))
        state
        (recur state)))))

(defn permutations [s]
  (lazy-seq
   (if (seq (rest s))
     (apply concat (for [x s]
                     (map #(cons x %) (permutations (remove #{x} s)))))
     [s])))

;; This doesn't actually let the programs terminate, just takes the first output
(defn run-amplifiers [state phases]
  (loop [phases phases input 0]
    (if-let [phase (first phases)]
      (recur (rest phases) (:output (run-program (prepare-program state []) [phase input])))
      input)))

;; This uses a fun memoize+atom trick to keep track of state of different amps over time
(defn run-loop-amplifiers [state phases]
  (let [amp (memoize (fn [phase]
                       (atom (prepare-program state [phase]))))]
    (loop [amps (cycle phases) input 0]
      (let [result (swap! (amp (first amps)) run-program [input])]
        (if (:terminated result)
          input ;; assumes the last output before termination was amp E
          (recur (rest amps) (:output result)))))))

(defn solve-1 [state]
  (apply max (map (partial run-amplifiers state) (permutations [0 1 2 3 4]))))

(defn solve-2 [state]
  (apply max (map (partial run-loop-amplifiers state) (permutations [5 6 7 8 9]))))

(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [state (map #(Integer/parseInt %) (str/split (first *command-line-args*) #","))]
    (println (solve-1 state))
    (println (solve-2 state))))
