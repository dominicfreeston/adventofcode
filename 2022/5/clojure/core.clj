(ns advent.2022.5
  (:require [clojure.string :as str]))

(comment
  (def ^:dynamic *command-line-args* ["../input.txt"])
  
  (parse-instruction "move 3 from 12 to 1\nmove 4 from 1 to 5")
  ,)

(defn parse-instructions [s]
  (map #(map parse-long (rest %))
       (re-seq #"move (\d+) from (\d+) to (\d+)" s)))

(defn parse-stacks [input]
  (let [rows (->> input
                  str/split-lines
                  (map (fn [line] (map
                                   (comp second (partial re-find #"\[(\w)\]") str/join)
                                   (partition-all 4 line))))
                  (take-while (partial not-every? nil?)))
        c (count (first rows))
        columns (map 
                 (fn [i] (->> rows
                              (apply concat)
                              (drop i)
                              (take-nth c)
                              (drop-while nil?)))
                 (range c))]
    (vec columns)))

(defn process-instruction-9000 [stacks [c s d]]
  (-> stacks
      (update (dec s) (partial drop c))
      (update (dec d) #(apply conj % (take c (nth stacks (dec s)))))))

(defn process-instruction-9001 [stacks [c s d]]
  (-> stacks
      (update (dec s) (partial drop c))
      (update (dec d) #(apply conj % (reverse (take c (nth stacks (dec s))))))))

(defn run-simulation [processor stacks instructions]
  (str/join (map first (reduce processor stacks instructions))))

(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [input (slurp (first *command-line-args*))
        instructions (parse-instructions input)
        stacks (parse-stacks input)]

    (println (run-simulation process-instruction-9000 stacks instructions))
    (println (run-simulation process-instruction-9001 stacks instructions))))
