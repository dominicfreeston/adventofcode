(ns advent.2019.11
  (:require [clojure.string :as str]))

;; Intcode computer carried over from day 9

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

(defn prepare-program
  [state input]
  {:memory (zipmap (range) state)
   :pointer 0
   :base 0
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

;; Day 11 Code

(defn setup-robot []
  {:hull {}
   :location [0 0]
   :directions (cycle [[0 -1]
                       [1 0]
                       [0 1]
                       [-1 0]])})

(defn next-dir [dirs rot]
  (case rot
    0 (drop 3 dirs)
    1 (drop 1 dirs)))

(defn update-robot [{:keys [hull location directions] :as state} color rotation]
  (let [directions (next-dir directions rotation)]
    (assoc state
           :hull (assoc hull location color)
           :location (mapv + location (first directions))
           :directions directions)))

(defn current-color [{:keys [hull location]}]
  (get hull location 0))

(defn run-full-program
  [program robot]
  (loop [program program robot robot outputs []]
    (case (count outputs)
      0 ;; Start next phase based with current color as input
      (let [result (run-program program [(current-color robot)])]
        (cond
          (:terminated result)
          (:hull robot)
          :else
          (recur result robot (conj outputs (:output result)))))
      1 ;; Get second out put but don't provide input (they stack up)
      (let [result (run-program program [])]
        (recur result robot (conj outputs (:output result))))
      2 ;; Update robot state based on program outputs
      (recur program (apply update-robot robot outputs) []))))

(defn solve-1 [state]
  (count (run-full-program (prepare-program state [])
                           (setup-robot))))

(defn solve-2 [state]
  (let [painted (run-full-program (prepare-program state [])
                                  (assoc (setup-robot) :hull {[0 0] 1}))
        max-x (apply max (map first (keys painted)))
        max-y (apply max (map second (keys painted)))]
    (partition (inc max-x) (for [y (range (inc max-y))
                                 x (range (inc max-x))]
                             (if (zero? (get painted [x y] 0)) "." "X")))))

(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [state (map #(Long/parseLong %) (str/split (first *command-line-args*) #","))]
    (println (solve-1 state))
    (doseq [line (solve-2 state)]
      (println line))))
