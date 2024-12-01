(ns advent-of-code.2023.day-13
  (:use [advent-of-code.core]))

(defn is-reflection? [[head & remaining] to-compare]
  (cond
    (= head (first to-compare)) (recur remaining (rest to-compare))
    (nil? (first to-compare)) true   ;de cima eh maior
    (nil? head) true                 ;de baixo eh maior
    :else false
    )
  )

(defn find-reflection [mirror]
  (loop [[first next :as mirror] mirror acc ()]
    (if (nil? first)
      nil
      (if (and (= first next) (is-reflection? (rest mirror) (conj acc first)))
        (conj acc first)
        (recur (rest mirror) (conj acc first))
        )))
  )

(defn summarize-reflection [mirror]
  (if-let [reflection (find-reflection mirror)]
    (* 100 (count reflection))
    (count (find-reflection (transpose mirror)))))

(defn parse
  [input]
  (loop [[head tail] (split-with-exclusive #(not-empty %) input)
         acc []]
    (if (empty? tail)
      (conj acc head)
      (recur (split-with-exclusive #(not-empty %) tail) (conj acc head)))))

(defn -main []
  (let [mirrors (parse (get-lines-2023 "day-13.txt"))]
    (reduce + (map summarize-reflection mirrors))
    )
  )
