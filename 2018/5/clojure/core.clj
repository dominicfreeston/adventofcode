(ns advent.2018.5
  (:require [clojure.string :as str]))

(defn pop-all [l]
  (loop [safe '() left l]
    (let [f (first left)
          s (second left)]
      (cond (empty? left) safe
            (nil? s) (conj safe f)
            :else (let [is-safe (or (= f s) (not (= (str/upper-case f) (str/upper-case s))))
                        n-safe (if is-safe (conj safe f) (rest safe))
                        n-left (if is-safe
                                 (rest left)
                                 (let [r-left (nthrest left 2)]
                                   (if (empty? safe) r-left (conj r-left (first safe)))))]
                    (recur n-safe n-left))))))

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
