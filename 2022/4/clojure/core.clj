(ns advent.2022.4
  (:require [clojure.string :as str]))

(comment
  (def *command-line-args* ["../input.txt"])
  
  (convert-to-range "5-43")
  (fully-contains (parse-assignments "100-101,5-100"))
  (overlaps (parse-assignments "101-101,5-100"))
  ,)

(defn convert-to-range [s]
  (map parse-long (str/split s #"-")))

(defn parse-assignments [s]
  (map convert-to-range (str/split s #",")))

(defn fully-contains [[[a b] [c d]]]
  (not (or (and (< a c) (< b d))
           (and (< c a) (< d b)))))

(defn overlaps [[[a b] [c d]]]
  (or (<= a c b)
      (<= a d b)
      (<= c a d)
      (<= c b d)))

(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [input (slurp (first *command-line-args*))
        ranges (map parse-assignments(str/split-lines input))]
    (println (count (filter fully-contains ranges)))
    (println (count (filter overlaps ranges)))))
