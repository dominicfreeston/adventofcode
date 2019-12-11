(ns advent.2019.10
  (:require [clojure.string :as str]
            [clojure.set :as s]))

(defn map-vals [f m]
  (into {} (map (juxt key (comp f val))) m))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (rem a b))))

(defn abs [v]
  (if (>= v 0) v (- v)))

(defn reduce-vector [[x y]]
  (let [d (abs (gcd x y))]
    [(/ x d) (/ y d)]))

(defn calc-vector [[x1 y1] [x2 y2]]
  [(- x2 x1) (- y2 y1)])

(defn calc-angle [[x y]]
  (let [angle (+ (/ Math/PI 2) (Math/atan2 y x))]
    (if (< angle 0)
      (+ (* 2 Math/PI) angle)
      angle)))

(defn dist [p1 p2]
  (reduce + (map abs (calc-vector p1 p2))))

(defn calc-visible [asteroids current]
  (->> (remove #{current} asteroids)
       (map (partial calc-vector current))
       (map reduce-vector)
       set
       count))

(defn find-loc [asteroids]
  (first (sort-by (partial calc-visible asteroids) > asteroids)))

;; Solutions

(defn solve-1 [asteroids]
  (calc-visible asteroids (find-loc asteroids)))

(defn solve-2 [asteroids]
  (let [base (find-loc asteroids)
        grouped (->> asteroids
                     (remove #{base})
                     (group-by (partial (comp calc-angle reduce-vector calc-vector) base))
                     (map-vals #(sort-by (partial dist base) %)))
        angles (take 200 (cycle (sort (keys grouped))))
        final (last angles)
        [x y] (nth (get grouped final) (dec (count (keep #{final} angles))))]
    (+ (* x 100) y)))

;; Map input to list of vectors

(defn parse-row [row y]
  (map #(if (= "#" %1) [%2 y] nil) row (range)))

(defn parse-input [input]
  (remove nil? (mapcat parse-row input (range))))

(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [input (map #(str/split % #"") *command-line-args*)]
    (println (solve-1 (parse-input input)))
    (println (solve-2 (parse-input input)))))

