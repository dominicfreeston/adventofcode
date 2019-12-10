(ns advent.2019.8
  (:require [clojure.string :as str]))

(defn layer-up [data]
  (partition 150 data))

(defn analyze [layer]
  (frequencies layer))

(defn find-layer [layers]
  (first (sort-by #((analyze %) 0) layers)))

(defn compute-result [layer]
  (let [info (analyze layer)]
    (* (info 1) (info 2))))

(defn solve-1 [data]
  (-> data
      layer-up
      find-layer
      compute-result))

(defn resolve-pixel [& values]
  (first (remove #{2} values)))

(defn resolve-layers [layers]
  (apply map resolve-pixel layers))

(defn beautify-pixel [value]
  (if (zero? value) "." "0"))

(defn solve-2 [data]
  (->> data
      layer-up
      resolve-layers
      (map beautify-pixel)
      (partition 25)))

(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [input (map #(Integer/parseInt (str %)) (first *command-line-args*))]
    (println (solve-1 input))
    (doseq [row (solve-2 input)]
      (println row))))
