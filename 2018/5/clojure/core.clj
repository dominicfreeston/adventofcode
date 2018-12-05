(ns advent.2018.5
  (:require [clojure.string :as str]))

(defn conjn [coll x]
  (if (nil? x) coll (conj coll x)))

(defn pop-all [l]
  (loop [safe '() [f s & left] l]
    (cond (nil? f) safe
          (nil? s) (conj safe f)
          :else (if (or (= f s) (not (= (str/upper-case f) (str/upper-case s))))
                  (recur (conj safe f) (conj left s))
                  (recur (rest safe) (conjn left (first safe)))))))

(def letters (map char (range (int \a) (inc (int \z)))))

(defn remove-char [l c]
  (filter #(not (= (str/upper-case c) (str/upper-case %))) l))

(defn remove-pop-count [l c]
  (count (pop-all (remove-char l c))))

(defn shortest-sequence [l]
  (apply min (map #(remove-pop-count l %) letters)))

(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [input (seq (first *command-line-args*))
        popped (pop-all input)]
    (println (count popped))
    (println (shortest-sequence input))))
