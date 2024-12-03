(ns advent-of-code.2024.day-2
  (:require [clojure.string :as str]
            [advent-of-code.2024.utils :as u]))

(declare safe-dump)
(def lines (u/get-lines "2024/day-2.txt"))
(defn parse-to-int-list [line] (map read-string (str/split line #" ")))
(defn parse [lines] (map parse-to-int-list lines))
(defn is-in-limit? [x y] (and (> (abs (- x y)) 0) (<= (abs (- x y)) 3)))
(defn is-safe? [f x y] (and (is-in-limit? x y) (f x y)))
(defn safe [f list] (safe-dump true f nil list))
(defn safe-dump
  ([f list]
   (safe-dump false f nil list))
  ([dump? f cache [x y & more]]
   (if (is-safe? f x y)
     (if (next more)
       (safe-dump dump? f x (cons y more))
       (if dump? (is-safe? f y (first more)) true))
     (if (not dump?)
       (or (when (nil? cache) (safe-dump true f nil (cons y more)))
           (if (next more) (safe-dump true f cache (cons x more))
                           (is-safe? f x (first more))))))))

(defn -main []
  (->> (parse lines)
       (map #(or (safe > %) (safe < %)))
       (filter true?) (count)
       (println))
  (->> (parse lines)
       (map #(or (safe-dump > %) (safe-dump < %)))
       (filter true?) (count)
       (println)))
