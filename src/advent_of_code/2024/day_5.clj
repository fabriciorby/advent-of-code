(ns advent-of-code.2024.day-5
  (:require [clojure.string :as str]
            [advent-of-code.2024.utils :as u]))

(def lines (u/get-lines "2024/day-5-example.txt"))
(defn split-to-int [string regex] (map read-string (str/split string regex)))
(defn get-info [[rules pages]] [(group-by first (map #(split-to-int % #"\|") rules)) (map #(split-to-int % #",") pages)])
(defn parse [lines] (get-info (u/split-with-exclusive #(not= "" %) lines)))
(parse lines)

(defn check-rule [[x y :as rule] page]
  (let [[a b :as full] (split-with #(not= x %) page)]
    (println rule full)
    (cond
      (empty? a) true
      :else (every? #(not= y %) a)
      ))
  )

(defn check [rules page]
  (when (every? true? (flatten (map (fn [c] (map #(check-rule % page) (get rules c))) page)))
    (nth page (quot (count page) 2))))

(defn process
  ([[rules pages]]
  (keep #(check rules %) pages)))


(defn check-rule-2 [[x y :as rule] page]
  (let [[a b :as full] (split-with #(not= x %) page)]
    (println rule full)
    (cond
      (empty? a) 0
      :else (if (every? #(not= y %) a)
              0
              ;(if (u/in? full x) x 0)
              x
              )
      ))
  )

(defn check-2 [rules page]

   (map (fn [c] (map #(check-rule-2 % page) (get rules c))) page))

(defn process-2
  ([[rules pages]]
   (keep #(check-2 rules %) pages)))

(->> (parse lines)
     (process-2)
     )

(defn -main [& args]
  (->> (parse lines)
       (process)
       (reduce +)
       (println))
  )
