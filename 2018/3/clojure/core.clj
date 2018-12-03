(ns advent.2018.3
  (:require [clojure.string :as str]))

;; Split string s using regular expression re
;; then convert each value to numbers
(defn to-num-list [s re]
  (map read-string (str/split s re)))

;; Parse the following:
;; "#1 @ 871,327: 16x20"
;; {:id 1 :x 871 :y 327 :w 16 :h 20}
(defn parse-shape [s]
  (let [parts (str/split s #" ")
        point (to-num-list (parts 2) #"[,:]")
        size (to-num-list (parts 3) #"x")]
    {:id (str/replace(parts 0) "#" "")
     :x (first point) :y (second point)
     :w (first size) :h (second size)}))

;; Create a list of [x, y] points based on
;; a single x and a list of ys
(defn create-points [x ys]
  (map (fn [y] [x, y]) ys))

;; Returns a list of all the squares (points)
;; contained in the area described by shape s
(defn squares-claimed [s]
  (let [x-vals (range (s :x) (+ (s :x) (s :w)))
        y-vals (range (s :y) (+ (s :y) (s :h)))]
    (mapcat #(create-points % y-vals) x-vals)))

;; Add all the points to map m, setting the val
;; to false if it did not exist yet and true otherwise
(defn add-points [m ps]
  (reduce (fn [m p]
            (if (nil? (get m p))
              (assoc m p false)
              (assoc m p true)))
          m
          ps))

;; Count all the overlapping claims in the map
(defn count-claims [claim-map]
  (reduce-kv (fn [total _ v] (if v (inc total) total))
             0
             claim-map))

(defn isUnique? [m shape]
  (let [squares (squares-claimed shape)]
    (not (reduce #(or %1 (get m %2)) false squares))))

(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [all-shapes (map parse-shape *command-line-args*)
        all-squares (map squares-claimed all-shapes)
        claim-map (reduce add-points {} all-squares)]
    (println (count-claims claim-map))
    (println ((first (filter #(isUnique? claim-map %) all-shapes)) :id))))
