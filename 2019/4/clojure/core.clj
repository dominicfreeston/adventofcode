(ns advent.2019.4
  (:require [clojure.string :as str]))

(defn split-code
  "Splits a number code into a list of digits"
  [num]
  (loop [result (list), n num]
    (if (pos? n)
      (recur (conj result (rem n 10))
             (quot n 10))
      result)))

(defn has-increasing-digits [code]
  (every? (fn [[a b]] (>= b a)) (partition 2 1 code)))

;; increasing-digit criteria ensures duplicate digits are adjacent

(defn has-multiple-same-digits [code]
  (boolean (some #(> % 1) (vals (frequencies code)))))

(defn has-exactly-two-same-digits [code]
  (boolean (some #{2} (vals (frequencies code)))))

(defn checker [& criteria]
  (fn [code]
    (let [split (split-code code)]
      (every? true? (map #(% split)
                         criteria)))))

(defn solve-1 [start end]
  (->> (range start end)
       (filter (checker has-increasing-digits
                        has-multiple-same-digits))
       count))

(defn solve-2 [start end]
  (->> (range start end)
       (filter (checker has-increasing-digits
                        has-exactly-two-same-digits))
       count))

(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [[start end] (map #(Integer/parseInt %)
                         (str/split (first *command-line-args*) #"-"))]
    (println (solve-1 start end))
    (println (solve-2 start end))))
