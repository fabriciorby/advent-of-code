(ns advent-of-code-2023.days.day-12
  (:use [advent-of-code-2023.core])
  (:require [clojure.string :as str]))

(defn parse [input]
  (let [[springs separations] (str/split input #"\s+")
        separations (str/split separations #",")]
    (list springs separations))
  )

(defn is-valid? [possibility separations]
  (let [regex
        (re-pattern
          (str "(\\.+)?" (str/join "(\\.+)" (map #(str "(#){" % "}") separations)) "(\\.+)?")
          )]
    (if-let [[result] (re-matches regex possibility)]
      (if (= result possibility) true false)
      false
      )
    )
  )

(defn valid-possibilities [possibilities separation]
  (count (filter true? (map #(is-valid? % separation) possibilities)))
  )

(defn all-possibilities
  ([[springs separations]]
   (println "All possibilities for" springs "and separation" separations)
   (valid-possibilities (map str/join (partition (count springs) (all-possibilities springs []))) separations))
  ([[spring & remaining] acc]
   (if (nil? spring)
     acc
     (if (= \? spring)
       (flatten (vector
                  (all-possibilities remaining (conj acc \.))
                  (all-possibilities remaining (conj acc \#))))
       (flatten (vector (all-possibilities remaining (conj acc spring))))
       )
     ))
  )

(defn part-2 [parsed]
  (map #(list (str/join "?" (repeat 5 (first %))) (flatten (repeat 5 (last %)))) parsed)
  )

(defn -main []
  (let [input (get-lines "day-12-example.txt")
        parsed (mapv parse input)
        parsed-2 (part-2 parsed)]
    (println (reduce + (map all-possibilities parsed)))
    (println (reduce + (map all-possibilities parsed-2)))
    )
  )