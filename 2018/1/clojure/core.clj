(ns advent.2018.1)

(defn find-repeat-freq [input]
  (loop [freq 0 found #{} nums (cycle input)]
    (let [new-freq (+ freq (first nums))]
      (if (contains? found new-freq)
        new-freq
        (recur new-freq (conj found new-freq) (rest nums))))))

(if (nil? *command-line-args*)
  (println "Specify the input")
  (let [nums (map read-string *command-line-args*)]
    (println (reduce + nums))
    (println (find-repeat-freq nums))))
