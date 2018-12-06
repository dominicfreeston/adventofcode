(ns advent.2018.6
  (:require [clojure.string :as str]))

;; Returns the absolute value of n
(defn abs [n]
  (if (neg? n) (-' n) n))

;; Calculate the linear distance between two points
(defn dist [a b]
  (abs (- a b)))

;; Calculate the manhattan distance between points a and b
;; Where points are stored as vectors [x y]
(defn man-dist [a b]
  (+ (dist (a 0) (b 0)) (dist (a 1) (b 1))))

;; Reduces the x and y value in a list of [x y]
;; to a single point [x y], applying f to
;; the x and y parts separately
(defn red-both [f s]
  (reduce (fn [m pt] [(f (m 0) (pt 0)) (f (m 1) (pt 1))]) s))

;; For a given [x y], return all the points
;; in the range [0 0] to [x y]
(defn all-points [[x y]]
  (let [all-x-points (fn [ys x] (map (fn [y] [x, y]) ys))]
    (mapcat (partial all-x-points (range (inc y))) (range (inc x)))))

;;; Part 1
;;;;;;;;;;

;; Get the value of the claim at pt for loc
(defn claim [pt loc]
  {:pt pt :loc loc :dist (man-dist pt loc)})

;; Returns the shortest claim, or a constested one if equal
(defn update-claim [cur new]
  (cond (nil? cur) new
        (= (:dist cur) (:dist new)) {:loc :contested :dist (:dist new) :pt (:pt new)}
        (< (:dist cur) (:dist new)) cur
        :else new))

;; Groups a list of claims by loc, returning a map
;; where loc is the key and the list of claims is the value,
;; ignoring all contested claims
(defn group-by-loc [claims]
  (let [acc (fn [m claim]
              (let [loc (claim :loc)]
                (assoc m loc (conj (m loc) claim))))]
    (dissoc (reduce acc {} claims) :contested)))

;; Returns a map of loc to successfully claimed point
(defn all-claims [pts locs]
  (let [best-claim #(reduce update-claim (map (partial claim %) locs))]
    (group-by-loc (map best-claim pts))))

;; Return whether a list of claims defines a finite region
;; If the claim owns an edge of the range then it is infinite
(defn is-finite [[mx my] claims]
  (let [lmax (red-both max (map :pt claims))
        lmin (red-both min (map :pt claims))]
    (and (> (lmin 0) 0) (> (lmin 1) 0) (< (lmax 0) mx) (< (lmax 1) my))))

;; Given a list of locations,
;; return the size of the largest finite  area
(defn largest-area [locs]
  (let [max-point (red-both max locs)
        claims (all-claims (all-points max-point) locs)
        finite (filter #(is-finite max-point (% 1)) claims)]
    (apply max (map #(count (% 1)) finite))))


;;; Part 2
;;;;;;;;;;

(defn safe-region-size [locs]
  (let [max-point (red-both max locs)
        is-in-region #(> 10000 (reduce + (map (partial man-dist %) locs)))]
    (count (filter is-in-region (all-points max-point)))))


;; Input Handling
;;;;;;;;;;;;;;;;;

;; Parse "x, y" into vector [x y]
(defn parse-loc [s]
  (mapv read-string (str/split s #", ")))

(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [locs (map parse-loc *command-line-args*)]
    (println (largest-area locs))
    (println (safe-region-size locs))))
