(ns advent-of-code.2025.day-11
  (:require [advent-of-code.2024.utils :as helper]
            [clojure.string :as str]))

(def lines (helper/get-lines "2025/day-11.txt"))
(defn parse-to-key-value [line] (str/split line #": "))
(defn parse [lines] (update-vals (into {} (map parse-to-key-value lines)) #(str/split % #" ")))

(defn walk-and-check
  ([parsed-lines] (walk-and-check parsed-lines "svr" nil nil))
  ([parsed-lines item fft dac]
   (cond
     (and (= item "out") fft dac) 1
     :else (->>
             (parsed-lines item)
             (map #(walk-and-check parsed-lines % (or fft (= item "fft")) (or dac (= item "dac"))))
             (reduce +)))))

(defn walk ([parsed-lines] (walk-and-check parsed-lines "you" 1 1)))

(alter-var-root #'walk-and-check memoize)                   ;caralhoooowww

(defn -main []
  (-> lines (parse) (walk) (println))
  (-> lines (parse) (walk-and-check) (println)))

