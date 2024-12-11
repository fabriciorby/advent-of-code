(ns advent-of-code.2024.day-6
  (:require [advent-of-code.2024.utils :as u]))

(def directions
  {\^ {:step [-1 0] :next \>}
   \> {:step [0 1] :next \v}
   \v {:step [1 0] :next \<}
   \< {:step [0 -1] :next \^}})
(def lines (u/get-lines "2024/day-6.txt"))
(def guard (u/find-item (to-array-2d lines) #(or (= % \^) (= % \>) (= % \<) (= % \v))))

(defn turn [guard] (:next (directions guard)))
(defn next-item ([m cur guard] (u/get-item-details m (map + cur (:step (directions guard))))))

(defn process
  ([[guard point] m] (process #{point} point (next-item m point guard) guard m))
  ([acc prev [x y item] guard m]
   (case item
     nil acc
     \# (recur acc prev (next-item m prev (turn guard)) (turn guard) m)
     (recur (conj acc [x y]) [x y] (next-item m [x y] guard) guard m))))

(defn has-loop
  ([prev guard detail m] (has-loop #{} prev (next-item m prev (turn guard)) (turn guard) detail m))
  ([collision prev [x y item :as detail] guard obstacle m]
   (cond
     (nil? item) nil
     (contains? collision [guard prev]) obstacle
     (or (= item \#) (= obstacle detail)) (recur (conj collision [guard prev]) prev (next-item m prev (turn guard)) (turn guard) obstacle m)
     :else (recur collision [x y] (next-item m [x y] guard) guard obstacle m))))

(defn process-2
  ([[guard point] m] (process-2 #{} #{} point (next-item m point guard) guard m))
  ([steps acc prev [x y item :as detail] guard m]
   (cond
     (nil? item) (disj acc nil)
     (= item \#) (recur steps acc prev (next-item m prev (turn guard)) (turn guard) m)
     (contains? steps [x y]) (recur (conj steps prev) acc [x y] (next-item m [x y] guard) guard m)
     :else (let [loop (has-loop prev guard detail m)]
             (recur (conj steps prev) (conj acc loop) [x y] (next-item m [x y] guard) guard m)))))


(defn -main [& args]
  (->> (to-array-2d lines)
       (process guard)
       (count) (println))
  (->> (to-array-2d lines)
       (process-2 guard)
       (count) (println)))

(-main)
