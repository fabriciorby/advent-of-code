(ns advent-of-code-2023.days.day-7
  (:require [advent-of-code-2023.core :as helper]
            [clojure.string :as str]))


(def cards "23456789TJQKA")
(def ranks '("Maior" "Dupla" "Duas Duplas" "Tripla" "Full House" "Quadrupla" "Quintupla"))

(defn parse [lines] (str/split lines #"\s+"))

(defn eval-card [card] (str/index-of cards card))

(defn eval-cards [cards] (mapv eval-card cards))

(defn eval-hand
  ([hand]
   (eval-hand (frequencies hand) {}))
  ([hand result]
   (if (empty? hand)
     result
     (let [higher-point (apply max-key val hand)]
       (if (empty? result)
         (cond
           (= (val higher-point) 5) (assoc result :rank 6)
           (= (val higher-point) 4) (assoc result :rank 5)
           (= (val higher-point) 3) (recur (dissoc hand (key higher-point)) (assoc result :rank 3))
           (= (val higher-point) 2) (recur (dissoc hand (key higher-point)) (assoc result :rank 1))
           (= (val higher-point) 1) (recur (dissoc hand (key higher-point)) (assoc result :rank 0)))
         (cond
           (and (= (:rank result) 3) (= (val higher-point) 2)) (assoc result :rank 4)
           (and (= (:rank result) 1) (= (val higher-point) 2)) (assoc result :rank 2)
           (and (= (:rank result) 0) (= (val higher-point) 1)) (recur (dissoc hand (key higher-point)) result)
           :else result)
         )
       )
     )
  )
  )

(defn sort-hands [calculated-hands] (sort-by (juxt :rank :hand) calculated-hands))

(defn eval-hands [input]
   (map #(assoc
           (assoc
             (eval-hand (map eval-card (first %)))
             :bid (Integer/parseInt (second %)))
           :hand (eval-cards (first %))) input)
  )

(defn print-hands [hands]
  (map #(assoc % :rank (nth ranks (:rank %))) hands)
  )

(defn -main []
  (let [lines (helper/get-lines "day-7.txt")
        input (map parse lines)]
    (reduce + (map-indexed #(* (inc %1) (:bid %2)) ((comp sort-hands eval-hands) input)))
    )
  )