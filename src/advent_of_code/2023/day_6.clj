(ns advent-of-code.2023.day-6
  (:require [advent-of-code.utils :as helper]
            [clojure.string :as str]))

(defn parse [lines]
  (letfn [(get-numbers [line] (map #(Integer/parseInt %) (rest (str/split line #"\s+"))))]
    {
     :times     (get-numbers (first lines))
     :distances (get-numbers (second lines))
     }
    )
  )

(defn parse-2 [lines]
  (letfn [(get-numbers [line] (Long/parseLong (str/join (rest (str/split line #"\s+")))))]
    {
     :time     (get-numbers (first lines))
     :distance (get-numbers (second lines))
     }
    )
  )

(defn calculate [time distance]
  (count (filter pos-int? (map - (map-indexed * (range time 0 -1)) (repeat time distance))))
  )

(defn -main []
  (let [lines (helper/get-lines "2023/day-6.txt")
        parsed-input (parse lines)
        parsed-input-2 (parse-2 lines)]
    (println (reduce * (map calculate (:times parsed-input) (:distances parsed-input))))
    (println (calculate (:time parsed-input-2) (:distance parsed-input-2)))
    )
  )
