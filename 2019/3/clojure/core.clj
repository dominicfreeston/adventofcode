(ns advent.2019.3
  (:require [clojure.string :as str]
            [clojure.set :as s]))

(defn add-vecs [v1 v2]
  (mapv + v1 v2))

(defn abs [v]
  (if (>= v 0) v (- v)))

(defn dir-vec [dir]
  (case dir
    "U" [0 1]
    "D" [0 -1]
    "L" [-1 0]
    "R" [1 0]))

(defn parse-wire-segment
  "Parses a wire segment string \"R12\" into a wire segment [R 12] of type [String Int]"
  [input]
  (let [[_ dir dist] (re-matches #"(\w)(\d+)" input)]
    [dir (Integer/parseInt dist)]))

(defn parse-wire
  "Takes a string and returns a list of wire-segments"
  [path]
  (map parse-wire-segment (str/split path #",")))

(defn segment-points
  "Converts a wire-segment into a list of points starting at start"
  [start [dir dist]]
  (map (partial add-vecs start)
       (reductions add-vecs
                   (take dist (repeat (dir-vec dir))))))

(defn wire-points
  "Converts a wire (a list of wire segments) into a list of all the segments points"
  [wire]
  (reduce (fn [all segment]
            (apply conj all (segment-points (or (peek all) [0 0]) segment)))
          []
          wire))

(defn manhattan-dist [point]
  (reduce + (map abs point)))

(defn steps-dist [wires point]
  (let [num-steps (fn [wire] (inc (count (take-while #(not= point %) wire))))]
    (apply + (map num-steps wires))))

(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [wires (map (comp wire-points parse-wire) *command-line-args*)
        cross-points (apply s/intersection (map set wires))]
    (println (apply min (map manhattan-dist cross-points)))
    (println (apply min (map (partial steps-dist wires) cross-points)))))

