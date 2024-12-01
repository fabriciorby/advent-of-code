(ns advent-of-code.2024.day-1
  (:require [advent-of-code.2024.utils :as helper]
            [clojure.string :as str]))

(def lines (helper/get-lines "2024/day-1.txt"))
(defn parse-to-int-pair [line] (map read-string (str/split line #"   ")))
(defn parse [lines] (->> (map parse-to-int-pair lines) (apply map list)))
(defn get-difference [lines] (->> (apply map - lines) (map abs)))
(defn get-similarity-score [[l1 l2]] (map #(* % (get (frequencies l2) % 0)) l1))

(defn -main []
  (->> (parse lines) (map sort)
       (get-difference) (reduce +)
       (println))
  (->> (parse lines)
       (get-similarity-score) (reduce +)
       (println)))