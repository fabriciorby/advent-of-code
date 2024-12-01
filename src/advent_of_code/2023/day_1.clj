(ns advent-of-code.2023.day-1
  (:require [clojure.string :as str])
  (:require [advent-of-code.utils :as helper])
  (:gen-class))

(def digits-map
  {"one"   "1",
   "two"   "2",
   "three" "3",
   "four"  "4",
   "five"  "5",
   "six"   "6",
   "seven" "7",
   "eight" "8",
   "nine"  "9"}
  )

(def regex-string (str/join "|" (keys digits-map)))

(def digits-map-reverse (update-keys digits-map str/reverse))

(defn get-first-digit [line]
  (if (Character/isDigit (char (first line)))
    (first line)
    (get-first-digit (rest line)))
  )

(defn get-secret-number-1 [line]
  (Integer/parseInt (str (get-first-digit line) (get-first-digit (reverse line)))))

(defn get-secret-number-2 [line]
  (Integer/parseInt (str
                      (get-first-digit (str/replace line (re-pattern regex-string) digits-map))
                      (get-first-digit (str/replace (str/reverse line) (re-pattern (str/reverse regex-string)) digits-map-reverse)))
                    )
  )

(defn -main []
  (let [lines (helper/get-lines "2023/day-1.txt")]
    (println (reduce + (map get-secret-number-1 lines)))
    (println (reduce + (map get-secret-number-2 lines)))
    )
  )
