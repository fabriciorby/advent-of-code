(ns advent-of-code-2023.days.day-3
  (:require [advent-of-code-2023.core :as helper]))

(defn parse [lines]
  (vec (map #(reduce conj [] %) lines)))

(defn is-symbol? [char]
  (if (every? #(not= char %) '(\% \/ \$ \& \* \@ \# \+ \- \=)) false true))

(defn is-not-digit? [char]
  (not (Character/isDigit char)))

(defn get-value [i j input]
  (if (and (>= i 0) (>= j 0) (< j (dec (count input))) (< i (dec (count (nth input j)))))
    (nth (nth input j) i)
    nil))

(defn is-symbol-near-char?
  ([i j input]
   (is-symbol-near-char? i j input -1 -1 false))
  ([i j input x y acc]
   (if (= x 1)
     (if (= y 1)
       (or acc (is-symbol? (get-value (+ i x) (+ j y) input)))
       (recur i j input -1 (inc y) (or acc (is-symbol? (get-value (+ i x) (+ j y) input)))))
     (recur i j input (inc x) y (or acc (is-symbol? (get-value (+ i x) (+ j y) input)))))))

(defn is-symbol-near-number?
  ([size i j input]
   (is-symbol-near-number? size (- i size) j input 1 false))
  ([size i j input cont acc]
   (if (= cont (inc size))
     acc
     (recur size (inc i) j input (inc cont) (or acc (is-symbol-near-char? i j input))))))

(defn sum-values [acc i j input]
  (if (is-symbol-near-number? (count (first acc)) i j input)
    (+ (Integer/parseInt (first acc)) (last acc))
    (last acc)))

(defn replace-acc [acc item]
  (conj (list (last acc)) (str (first acc) item))
  )

(defn replace-sum [acc i j input]
  (conj (list (sum-values acc i j input)) "")
  )

(defn calculate
  ([input]
   (calculate 0 0 input '("" 0)))
  ([i j input acc]
   (let [line (nth input j) item (nth line i)]
     (if (= i (- (count line) 1))
       (if (= j (- (count input) 1))
         (last acc)
         (if (is-not-digit? item)
           (recur 0 (inc j) input (replace-sum acc i j input))
           (recur 0 (inc j) input (replace-sum (replace-acc acc item) i j input))))
       (if (is-not-digit? item)
         (if (= (first acc) "")
           (recur (inc i) j input acc)
           (recur (inc i) j input (replace-sum acc i j input)))
         (recur (inc i) j input (replace-acc acc item)))))))

(defn -main []
  (let [lines (helper/get-lines "day-3.txt")]
    (let [parsed-input (parse lines)]
      (println (calculate parsed-input))))
  )
