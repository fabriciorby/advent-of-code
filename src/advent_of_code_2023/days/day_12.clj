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
   (map str/join (partition (count springs) (all-possibilities springs []))))
  ([[spring & remaining] acc]
   (if (nil? spring)
     acc
     (if (= \? spring)
       (flatten (vector
                  (all-possibilities remaining (conj acc \.))
                  (all-possibilities remaining (conj acc \#))))
       (vector (all-possibilities remaining (conj acc spring)))
       )
     ))
  )

(defn valid [possibility separations]

  ;; check with regex (no internet) :'(

  )

(defn -main []
  (let [input (get-lines "day-12-example.txt")
        parsed (mapv parse input)]
    ;(map all-possibilities parsed)
    (all-possibilities (first (last parsed)))
    )
  )