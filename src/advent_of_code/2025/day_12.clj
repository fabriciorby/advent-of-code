(ns advent-of-code.2025.day-12
  (:require [clojure.string :as str])
  (:use [advent-of-code.2024.utils]))

(def lines (get-lines "2025/day-12.txt"))
(defn split-input-to-list [lines] (split-by-exclusive #(= % "") lines))
(defn split-grids [line] (str/split line #": "))
(defn find-present-coordinates [i-map] (map last (find-items i-map #(= \# %))))

(defn parse [lines]
  (let [parsed-lines (split-input-to-list lines)]
    {
     :presents (->> parsed-lines butlast (map rest) (map to-array-2d) (map find-present-coordinates) (map set))
     :grids    (->> parsed-lines last (map split-grids)
                    (map #(list
                            (map read-string (str/split (first %) #"x"))
                            (map read-string (str/split (second %) #" ")))))
     })
  )

(defn check-area
  ([input] (reduce + (map #(check-area (first %) (second %)) (:grids input))))
  ([grid-size grid-config]
   (let [area (reduce * grid-size) sum (reduce + grid-config)]
     (if (>= area (* 9 sum)) 1 0)))
  )

(defn -main []
  (-> lines (parse) (check-area) (println)))
