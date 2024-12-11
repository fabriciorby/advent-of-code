(ns advent-of-code.2024.day-4
  (:require [advent-of-code.2024.utils :as u]))

(defn iterate-matrix [f matrix] (map-indexed #(keep-indexed (fn [j v] (f matrix [% j v])) %2) matrix))
(def all-directions (for [x [1 0 -1] y [1 0 -1] :when (not= [x y] [0 0])] [x y]))
(def all-diagonals (for [x [1 -1] y [1 -1]] [x y]))
(defn next-item [m cur step] (u/get-item-details m (mapv + cur step)))
(defn prev-item [m cur step] (u/get-item-details m (mapv + cur (map #(* -1 %) step))))
(defn find-words
  ([m [h & t] step [_ _ v :as item]]
   (cond
     (nil? h) 1
     (not= v h) nil
     (nil? step) (if (= h \X) (->> all-directions (keep #(find-words m "XMAS" % item)) (reduce +))
                              (->> all-diagonals (keep #(find-words m "MAS" % (prev-item m item %))) (reduce +)))
     :else (->> (next-item m item step) (find-words m t step)))))

(defn is-xmas?  ([m item] (find-words m "X" nil item)))
(defn is-x-mas? ([m item] (find-words m "A" nil item)))
(def lines (u/get-lines "2024/day-4.txt"))
(defn -main [& args]
  (->> (to-array-2d lines) (iterate-matrix is-xmas?)
       (flatten) (reduce +) (println))
  (->> (to-array-2d lines) (iterate-matrix is-x-mas?)
       (flatten) (filter #(= 2 %)) (count) (println)))
