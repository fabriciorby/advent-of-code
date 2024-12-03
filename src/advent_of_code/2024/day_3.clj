(ns advent-of-code.2024.day-3
  (:require [advent-of-code.2024.utils :as u]))

(defn parse [input] (apply str input))
(defn mul [x y] (* (read-string x) (read-string y)))
(defn process ([operations] (process operations true 0))
  ([[[op x y] & tail] flag acc]
   (cond (nil? op) (println acc)
         (= op "do()") (recur tail true acc)
         (= op "don't()") (recur tail false acc)
         (not flag) (recur tail flag acc)
         :else (recur tail flag (+ acc (mul x y))))))

(defn -main [& args]
  (def lines (u/get-lines "2024/day-3.txt"))
  (->> (parse lines) (re-seq #"mul\((\d+),(\d+)\)") (process))
  (->> (parse lines) (re-seq #"mul\((\d+),(\d+)\)|do\(\)|don't\(\)") (process)))
