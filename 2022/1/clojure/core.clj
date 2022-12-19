(ns advent.2022.1
  (:require [clojure.string :as str]))

(comment
  ;; evaluate this if running the code below from a repl started from this file
  (def *command-line-args* ["../input.txt"])
  ,)

(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [input (slurp (first *command-line-args*))
        elves (->> (str/split input #"\n\n")
                   (map #(map parse-long (str/split % #"\n"))))
        calories (map (partial apply +) elves)]
    (println (apply max calories))
    (println (apply + (take 3 (sort > calories))))))

