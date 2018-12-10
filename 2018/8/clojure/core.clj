(ns advent.2018.8
  (:require [clojure.string :as str]))

(defn parse-node [[n m & l]]
  (declare get-children)
  (if (= n 0)
    {:node {:meta (take m l) :children nil} :rest (nthrest l m)}
    (let [result (get-children n l)
          children (result :children)
          l (result :rest)]
      {:node {:meta (take m l) :children children} :rest (nthrest l m)})))

(defn get-children [c l]
  (loop [children [] c c l l]
    (if (= 0 c)
      {:children children :rest l}
      (let [result (parse-node l)]
        (recur (conj children (result :node)) (dec c) (result :rest))))))

(defn get-meta [{:keys [children meta]}]
  (if children
    (reduce #(concat (get-meta %2) %1) meta children)
    meta))

(defn get-val [{:keys [children meta]}]
  (if children
    (let [meta-children (map #(get children (dec %) {:meta '()}) meta)]
      (reduce + (map get-val meta-children)))
    (reduce + meta)))

(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [line (first *command-line-args*)
        res (str/split line #" ")
        input (map read-string res)
        tree (:node (parse-node input))]
    (println (reduce + (get-meta tree)))
    (println (get-val tree))))
