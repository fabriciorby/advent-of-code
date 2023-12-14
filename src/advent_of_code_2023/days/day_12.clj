(ns advent-of-code-2023.days.day-12
  (:use [advent-of-code-2023.core])
  (:require [clojure.string :as str]))

(defn parse [input]
  (let [[springs separations] (str/split input #"\s+")
        separations (str/split separations #",")]
    (list springs separations))
  )

(comment
  (defn calculate-all
    ([points]
     (calculate-all points 0))
    ([[first & tail] i]
     (if (not-empty tail)
       (recur tail (reduce #(+ %1 (calculate-distance first %2)) i tail))
       i))
    )
  )

(defn calculate-fitness [[spring separations]]
  (loop [[separation & remaining] separations
         sublist ()]
    (recur remaining sublist)
    )
  spring
  )

(defn -main []
  (let [input (get-lines "day-12-example.txt")
        parsed (map parse input)]
    (map calculate-fitness parsed)
    )
  )