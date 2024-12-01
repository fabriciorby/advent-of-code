(ns advent-of-code.2023.day-12
  (:use [advent-of-code.utils])
  (:require [clojure.string :as str]))

(defn parse [input]
  (let [[springs separations] (str/split input #"\s+")
        separations (map #(Integer/parseInt %) (str/split separations #","))]
    (list springs separations))
  )

(defn count-possibilities
  ([[springs separations]]
   (count-possibilities springs separations))
  ([[spring & remaining :as springs] [separation :as separations]]
   (cond
     (nil? spring) (if (empty? separations) 1 0)
     (empty? separations) (if (in? springs \#) 0 1)
     :else
     (+
        (if (or (= \? spring) (= \. spring))
          (count-possibilities remaining separations) 0)
        (if (and
              (or (= \? spring) (= \# spring))
              (and
                (<= separation (count springs))
                (not (in? (take separation springs) \.))
                (or
                  (= separation (count springs))
                  (not= (nth springs separation) \#))))
          (count-possibilities (drop separation remaining) (rest separations)) 0)
        )
     )
   )
  )

(defn part-2 [parsed]
  (map #(list (str/join "?" (repeat 5 (first %))) (flatten (repeat 5 (last %)))) parsed))

(alter-var-root #'count-possibilities memoize)              ;caralhoooowww

(defn -main []
  (let [input (get-lines "2023/day-12.txt")
        parsed (mapv parse input)
        parsed-2 (part-2 parsed)]
    (println (reduce + (map count-possibilities parsed)))
    (println (reduce + (map count-possibilities parsed-2)))
    )
  )
