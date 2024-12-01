(ns advent-of-code.2023.day-9
  (:use [advent-of-code.core])
  (:require [clojure.string :as str]))

(defn parse [line]
  (map #(Integer/parseInt %) (str/split line #"\s+")))

(defn generate-child
  ([values]
   (let [[first second :as values] values
         child (- second first)]
     (generate-child (rest values) (vector child))))
  ([values children]
   (let [[first second :as values] values
         child (if (nil? second) 0 (- second first))]
     (if (nil? second)
       children
       (recur (rest values) (conj children child)))))
  )

(defn sum-future [children] (reduce #(+ (last %2) %1) 0 children))

(defn sum-previous [children] (reduce #(- (first %2) %1) 0 (reverse children)))

(defn generate-result [children f]
  (if (every? zero? (last children))
    (f children)
    (recur (conj children (generate-child (last children))) f)
    )
  )

(defn -main []
  (let [lines (get-lines-2023 "day-9.txt")
        input (map parse lines)]
    (println (reduce + (map #(generate-result (vector %) sum-future) input))) ;part 1
    (println (reduce + (mapv #(generate-result (vector %) sum-previous) input))) ;part 2
    (println (reduce + (mapv #(generate-result (vector (reverse %)) sum-future) input))) ;part 2 bonus
    )
  )
