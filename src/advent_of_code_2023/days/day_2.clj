(ns advent-of-code-2023.days.day_2
  (:require [advent-of-code-2023.core :as helper]
            [clojure.string :as str]))

(defn generate-map [map set]
  (let [trim-set (str/split (str/trim set) #" ")]
    (into map (hash-map (keyword (last trim-set)) (Integer/parseInt (first trim-set)))))
  )

(defn separate-set [set] (reduce generate-map {} (str/split set #",")))

(defn parse [line] (map separate-set (str/split (last (str/split line #":")) #";")))

(defn subtract-input [parsed-input]
  (map #(merge-with - {:red 12, :blue 14, :green 13} %) parsed-input)
  )

(defn get-possible-games-indexes [index subtracted-input]
  (if (reduce #(if (every? nat-int? (vals %2)) (and true %1) false) true subtracted-input) (+ index 1) 0)
  )

(defn get-power-of-maxes [maxed-input]
   (apply * (vals maxed-input))
  )

(defn -main []
  (let [lines (helper/get-lines "day-2.txt")]
    (let [parsed-input (map parse lines)]
      (println (reduce + (map-indexed get-possible-games-indexes (map subtract-input parsed-input))))
      (println (reduce + (map get-power-of-maxes (map #(apply merge-with max %) parsed-input))))
      )
    )
  )
