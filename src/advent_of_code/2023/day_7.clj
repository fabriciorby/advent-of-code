(ns advent-of-code.2023.day-7
  (:require [advent-of-code.core :as helper]
            [clojure.string :as str]))

(defn parse [lines] (str/split lines #"\s+"))

(defn eval-cards [cards] (mapv #(str/index-of "23456789TJQKA" %) cards))

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
             (eval-hand (eval-cards (first %)))
             :bid (Integer/parseInt (second %)))
           :hand (eval-cards (first %))) input)
  )

(defn replace-high-card-with-joker [hand]
  (if (= "JJJJJ" hand)
    hand
    (let [high-card (apply max-key val (dissoc (frequencies hand) \J))]
      (str/replace hand #"J" (str (key high-card)))
      )
    )
  )

(defn eval-cards-2 [cards] (mapv #(str/index-of "J23456789TQKA" %) cards))

(defn eval-hands-2 [input]
  (map #(assoc
          (assoc
            (eval-hand (eval-cards (replace-high-card-with-joker (first %))))
            :bid (Integer/parseInt (second %)))
          :hand (eval-cards-2 (first %))) input)
  )

(defn -main []
  (let [lines (helper/get-lines "2023/day-7.txt")
        input (map parse lines)]
    (println (reduce + (map-indexed #(* (inc %1) (:bid %2)) (sort-hands (eval-hands input)))))
    (println (reduce + (map-indexed #(* (inc %1) (:bid %2)) (sort-hands (eval-hands-2 input)))))
    )
  )
