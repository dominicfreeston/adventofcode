(ns advent.2018.3
  (:require [clojure.string :as str]
            [clojure.set :as st]))

;; Split string s using regular expression re
;; then convert each value to numbers
(defn to-num-list [s re]
  (map read-string (str/split s re)))

;; Parse the following:
;; "#1 @ 871,327: 16x20"
;; {:id 1 :x 871 :y 327 :w 16 :h 20}
(defn parse-shape [s]
  (let [parts (str/split s #" ")
        square (to-num-list (parts 2) #"[,:]")
        size (to-num-list (parts 3) #"x")]
    {:id (str/replace(parts 0) "#" "")
     :x (first square) :y (second square)
     :w (first size) :h (second size)}))

;; Create a list of [x, y] squares based on
;; a single x and a list of ys
(defn create-squares [x ys]
  (map (fn [y] [x, y]) ys))

;; Returns a list of all the squares (squares)
;; contained in the area described by shape s
(defn squares-claimed [s]
  (let [x-vals (range (s :x) (+ (s :x) (s :w)))
        y-vals (range (s :y) (+ (s :y) (s :h)))]
    (mapcat #(create-squares % y-vals) x-vals)))

;; Add all the squares to squares-map, setting the val
;; to true if it already exists, false otherwise
(defn add-squares [squares-map squares-list]
  (reduce (fn [m sq]
            (if (nil? (m sq))
              (assoc m sq false)
              (assoc m sq true)))
          squares-map
          squares-list))

;; Returns a set containing all contended squares,
;; That is any contained in multiple shapes in all-shapes
(defn contended-squares [all-shapes]
  (let [all-squares (map squares-claimed all-shapes)
        squares-map (reduce add-squares {} all-squares)]
    (reduce (fn [s v] (if (v 1) (conj s (v 0)) s))
            #{}
            squares-map)))

(defn unique-shape? [contended shape]
  (let [squares (set (squares-claimed shape))]
    (empty? (st/intersection squares contended))))

(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [all-shapes (map parse-shape *command-line-args*)
        contended (contended-squares all-shapes)]
    (println (count contended))
    (println ((first (filter #(unique-shape? contended %) all-shapes)) :id))))
