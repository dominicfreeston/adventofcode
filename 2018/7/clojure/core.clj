(ns advent.2018.7
  (:require [clojure.string :as str]))

(defn add-dep [info n d]
  (let [info (assoc info d (info d #{}))]
    (assoc info n (conj (info n #{}) d))))

(defn process-deps [l]
   (reduce (fn [m v] (add-dep m (v :node) (v :dep))) {} l))

(defn find-avail [info]
  (sort (keys (filter #(empty? (% 1)) info))))

(defn find-next [info]
  (first (find-avail info)))

(defn remove-node [info n]
  (reduce-kv (fn [m k v] (assoc m k (disj v n #{}))) {} (dissoc info n)))

;; Part 1

(defn resolve-order [info]
  (loop [info info result ""]
    (if (empty? info)
      result
      (let [node (first (find-avail info))]
        (recur (remove-node info node) (str result node))))))


;; Part 2

(defn time-needed [c]
  (- (int (.charAt c 0)) 4))

(defn start-node [acc n]
  (-> acc
      (assoc-in [:curr n] (time-needed n))
      (assoc :deps (dissoc (acc :deps) n))))

(defn start-possible-nodes [acc]
  (loop [acc acc]
    (let [node (find-next (acc :deps))]
      (if (or (= (count (acc :curr)) 5) (nil? node))
        acc
        (recur (start-node acc node))))))

(defn pass-time [acc]
  (let [min-val (apply min (map #(% 1) (acc :curr)))
        ntotal (+ min-val (acc :total))
        ncurr (reduce-kv (fn [m k v] (assoc m k (- v min-val))) {} (acc :curr))]
    (assoc acc :total ntotal :curr ncurr)))

(defn shift-next [acc]
  (let [finished ((first (filter #(= 0 (% 1)) (acc :curr))) 0)
        ncurr (dissoc (acc :curr) finished)
        ndeps (remove-node (acc :deps) finished)]
    (assoc acc :curr ncurr :deps ndeps)))

(defn resolve-time [info]
  (loop [acc {:total 0 :curr {} :deps info}]
    (if (and (empty? (acc :deps)) (empty? (acc :curr)))
      (acc :total)
      (recur (-> acc
                 start-possible-nodes
                 pass-time
                 shift-next)))))

;; Results
(defn parse-line [line]
  (let [res (str/split line #" ")
        dep (res 1)
        node (res 7)]
    {:node node :dep dep}))

(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [deps (map parse-line *command-line-args*)
        info (process-deps deps)]
    (println (resolve-order info))
    (println (resolve-time info))))
