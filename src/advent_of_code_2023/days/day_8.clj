(ns advent-of-code-2023.days.day-8
  (:use [advent-of-code-2023.core])
  (:require [clojure.string :as str]))

(defn parse [lines]
  {
   :directions (cycle (first lines))               ;cuidado pra nunca mandar printar essa merda
   :decisions  (reduce
                 #(into %1 {(subs %2 0 3) {\L (subs %2 7 10) \R (subs %2 12 15)}})
                 (sorted-map)
                 (nthrest lines 2))
   }
  )

(defn go-your-own-way
  ([input]
   (go-your-own-way (:directions input) (:decisions input) "AAA" 0))
  ([directions decisions current steps]
   (if (= current "ZZZ")
     steps
     (let [[next & remaining] directions]
       (recur remaining decisions (get (get decisions current) next) (inc steps))
       ))
   )
  )

(defn ghost-your-own-way
  ([input]
   (let [starting-points (filter #(str/ends-with? % "A") (keys (:decisions input)))]
     (ghost-your-own-way (:directions input) (:decisions input) starting-points 0)))
  ([directions decisions current steps]
   (if (every? #(str/ends-with? % "Z") current)
     steps
     (let [[next & remaining] directions]
       (if (= (rem (inc steps) 1000000) 0) (println (inc steps)))
       (recur remaining decisions (doall (map #(get (get decisions %) next) current)) (inc steps))
       ))
   )
  )

(defn -main []
  (let [lines (get-lines "day-8.txt")
        input (parse lines)]
    (println (go-your-own-way input))
    (println (ghost-your-own-way input))                    ;brute too strong never finishes
    )
  )