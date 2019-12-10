(ns advent.2019.9
  (:require [clojure.string :as str]))

(defn readval
  "Reads a parameter value from memory using the appropriate mode"
  [{:keys [memory pointer modes base]} param]
  (let [mode (get modes (dec param) 0)]
   (case mode
     0 (get memory (get memory (+ pointer param) 0) 0)
     1 (get memory (+ pointer param) 0)
     2 (get memory (+ base (get memory (+ pointer param) 0)) 0))))

(defn writeval
  "Returns the location to write to based on the appropriate mode"
  [{:keys [memory pointer modes base]} param]
  (let [mode (get modes (dec param) 0)]
   (case mode
     0 (get memory (+ pointer param) 0)
     1 (throw (Exception. "Immediate mode invalid when writing"))
     2 (+ base (get memory (+ pointer param) 0)))))

;; Operations

(defn add-op [{:keys [memory pointer] :as state}]
  (assoc state
         :memory (assoc memory
                        (writeval state 3)
                        (+ (readval state 1) (readval state 2)))
         :pointer (+ pointer 4)))

(defn mult-op [{:keys [memory pointer] :as state}]
  (assoc state
         :memory (assoc memory
                        (writeval state 3)
                        (* (readval state 1) (readval state 2)))
         :pointer (+ pointer 4)))

(defn input-op [{:keys [memory input pointer] :as state}]
  (assoc state
         :memory (assoc memory
                        (writeval state 1)
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
                        (writeval state 3)
                        (if (< (readval state 1) (readval state 2)) 1 0))
         :pointer (+ pointer 4)))

(defn equals-op [{:keys [memory pointer] :as state}]
  (assoc state
         :memory (assoc memory
                        (writeval state 3)
                        (if (= (readval state 1) (readval state 2)) 1 0))
         :pointer (+ pointer 4)))

(defn adjust-base [{:keys [base pointer] :as state}]
  (assoc state
         :base (+ base (readval state 1))
         :pointer (+ pointer 2)))

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
    9 adjust-base
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

;; Main run loop

;; We now use a map to represent memory instead of a vector
;; as it's easier to simulate "infinite memory", we just don't store the gaps
(defn prepare-program
  [state input]
  {:memory (zipmap (range) state)
   :pointer 0
   :base 0
   :input input
   :output nil})        

;; run program interrupts each time there's an output (this was necessary on day 7)
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

;; This is equivalent close to the original run-program on day 5
;; Runs the program to completion and returns a vector of outputs
;; But the program must have been prepared (with any necessary inputs)
(defn run-full-program
  [program]
  (loop [program program outputs []]
    (let [result (run-program program [])]
      (if (:terminated result)
        outputs
        (recur result (conj outputs (:output result)))))))

(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [state (map #(Integer/parseInt %) (str/split (first *command-line-args*) #","))]
    (println (first (run-full-program (prepare-program state [1]))))
    (println (first (run-full-program (prepare-program state [2]))))))



