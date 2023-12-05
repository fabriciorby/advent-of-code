(ns advent-of-code-2023.days.day-4
  (:require [advent-of-code-2023.core :as helper]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn split-and-parse-int [items]
   (set (str/split items #"\s+"))
  )

(defn get-points [items]
  (let [size (count items)]
    (if (> size 0) (reduce * (repeat (dec size) 2)) 0)
    )
  )

(defn generate-sum-list [card copies index]
  (let [all-cards (nthrest copies index)]
    (if (< (count card) (count all-cards))
      (concat (repeat (count card) (nth copies (dec index))) (repeat (- (count all-cards) (count card)) 0))
      )
    )
  )

(defn update-sum [card copies index]
  (map + (nthrest copies (inc index)) (generate-sum-list card copies (inc index)))
  )

(defn get-copies
  ([cards]
   (get-copies cards (repeat (count cards) 1) 0))
  ([cards copies index]
   (let [card (nth cards index)]
     (if (= index (dec (count cards)))
       copies
       (get-copies cards (concat (take (inc index) copies) (update-sum card copies index)) (inc index)))))
  )

(defn parse [line] (map split-and-parse-int (str/split (str/trim (last (str/split line #":"))) #" \| ")))
(defn -main []
  (let [lines (helper/get-lines "day-4.txt") parsed-input (map parse lines)]
    (println (reduce + (map #(get-points (set/intersection (first %) (last %))) parsed-input)))
    (println (reduce + (get-copies (map #(set/intersection (first %) (last %)) parsed-input))))
    )
  )