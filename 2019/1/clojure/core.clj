(ns advent.2019.1)

(defn calc-fuel [mass]
  (int (- (Math/floor (/ mass 3)) 2)))

(defn calc-total-fuel [mass]
  (loop [mass mass fuel 0]
    (let [new (calc-fuel mass)]
      (if (> new 0)
        (recur new (+ fuel new))
        fuel))))

(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [nums (map #(Integer/parseInt %) *command-line-args*)]
    (println (reduce + (map calc-fuel nums)))
    (println (reduce + (map calc-total-fuel nums)))))
