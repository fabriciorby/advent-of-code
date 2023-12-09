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
   (go-your-own-way (:directions input) (:decisions input) "AAA" #(= "ZZZ" %) 0))
  ([input start]
   (go-your-own-way (:directions input) (:decisions input) start #(= "ZZZ" %) 0))
  ([input start end]
   (go-your-own-way (:directions input) (:decisions input) start end 0))
  ([directions decisions current end steps]
   (if (end current)
     steps
     (let [[next & remaining] directions]
       (recur remaining decisions (get (get decisions current) next) end (inc steps))
       ))
   )
  )

(defn gcd [a b] (if (= 0 b) a (recur b (mod a b))))
(defn lcm [a b] (/ (* a b) (gcd a b)))

(defn ends-with-z? [word] (str/ends-with? word "Z"))

(defn ghost-your-own-way
  ([input]
   (let [starting-points (filter #(str/ends-with? % "A") (keys (:decisions input)))]
     (reduce lcm (map #(go-your-own-way input % ends-with-z?) starting-points))
     )))

(defn -main []
  (let [lines (get-lines "day-8.txt")
        input (parse lines)]
    (println (go-your-own-way input))
    (println (ghost-your-own-way input))
    )
  )