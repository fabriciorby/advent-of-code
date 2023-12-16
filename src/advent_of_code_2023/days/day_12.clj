(ns advent-of-code-2023.days.day-12
  (:use [advent-of-code-2023.core])
  (:require [clojure.string :as str]))

(defn parse [input]
  (let [[springs separations] (str/split input #"\s+")
        separations (str/split separations #",")]
    (list springs separations))
  )

(defn all-possibilities
  ([springs]
   (println "All possibilities for " springs)
   (map str/join (partition (count springs) (all-possibilities springs []))))
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

(defn -main []
  (let [input (get-lines "day-12.txt")
        parsed (mapv parse input)
        possibilities (map first parsed)
        separations (map second parsed)]
    (println (reduce + (map valid-possibilities (map all-possibilities parsed) separations)))
    )
  )