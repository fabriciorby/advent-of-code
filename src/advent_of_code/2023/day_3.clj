(ns advent-of-code.2023.day-3
  (:require [advent-of-code.core :as helper]
            [clojure.string :as str]))

(defn parse [lines]
  (vec (map #(reduce conj [] %) lines)))

(defn is-symbol? [char]
  (if (every? #(not= char %) '(\% \/ \$ \& \* \@ \# \+ \- \=)) false true))

(defn is-digit? [char]
  (if char (Character/isDigit char) false))

(defn is-not-digit? [char]
  (not (is-digit? char)))

(defn get-value [i j input]
  (if (and (>= i 0) (>= j 0) (<= j (dec (count input))) (<= i (dec (count (nth input j)))))
    (nth (nth input j) i)
    nil))

(defn is-xxx-near-char?
  ([i j input is-xxx?]
   (is-xxx-near-char? i j input -1 -1 false is-xxx?))
  ([i j input x y acc is-xxx?]
   (if (= x 1)
     (if (= y 1)
       (or acc (is-xxx? (get-value (+ i x) (+ j y) input)))
       (recur i j input -1 (inc y) (or acc (is-xxx? (get-value (+ i x) (+ j y) input))) is-xxx?))
     (recur i j input (inc x) y (or acc (is-xxx? (get-value (+ i x) (+ j y) input))) is-xxx?)))
  )

(defn is-symbol-near-char?
  ([i j input]
   (is-xxx-near-char? i j input -1 -1 false is-symbol?))
  )

(defn is-digit-near-char?
  ([i j input]
   (is-xxx-near-char? i j input -1 -1 false is-digit?))
  )

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

(defn is-gear? [char]
  (= char \*)
  )

(defn find-first-digit-index [i j input]
  (if (is-not-digit? (get-value i j input))
    (inc i)
    (recur (dec i) j input)
    )
  )

(defn find-last-digit-index [i j input]
  (if (is-not-digit? (get-value i j input))
    (dec i)
    (recur (inc i) j input)
    )
  )

(defn find-full-number [i j x y input]
  (take-while is-digit? (nthrest (nth input (+ j y)) (find-first-digit-index (+ i x) (+ j y) input)))
  )

(defn find-x-offset [i j x y input]
  (- (find-last-digit-index (+ i x) (+ j y) input) (+ i x))
  )

(defn get-number-near-char
  ([i j input]
   (get-number-near-char i j input -1 -1 ()))
  ([i j input x y acc]
   (if (>= x 1)
     (if (= y 1)
       (if (and (= x 1) (is-digit? (get-value (+ i x) (+ j y) input)))
         (map #(Integer/parseInt %) (map str/join (conj acc (find-full-number i j x y input))))
         (map #(Integer/parseInt %) (map str/join acc)))
       (if (and (= x 1) (is-digit? (get-value (+ i x) (+ j y) input)))
         (recur i j input -1 (inc y) (conj acc (find-full-number i j x y input)))
         (recur i j input -1 (inc y) acc)))
     (if (is-digit? (get-value (+ i x) (+ j y) input))
       (recur i j input (inc (find-x-offset i j x y input)) y (conj acc (find-full-number i j x y input)))
       (recur i j input (inc x) y acc))
     ))
  )

(defn gear-ratio [i j input]
  (if (is-digit-near-char? i j input)
    (let [numbers-near (get-number-near-char i j input)]
      (if (= 2 (count numbers-near))
        (* (first numbers-near) (second numbers-near)) 0))
    0)
  )

(defn calculate-gears
  ([input]
   (calculate-gears 0 0 input 0))
  ([i j input acc]
   (let [line (nth input j) item (nth line i)]
     (if (= i (- (count line) 1))
       (if (= j (- (count input) 1))
         acc
         (if (is-gear? item)
           (recur 0 (inc j) input (+ acc (gear-ratio i j input)))
           (recur 0 (inc j) input acc)))
       (if (is-gear? item)
         (recur (inc i) j input (+ acc (gear-ratio i j input)))
         (recur (inc i) j input acc)))
     )
   ))

(defn -main []
  (let [lines (helper/get-lines "2023/day-3.txt")]
    (let [parsed-input (parse lines)]
      (println (calculate parsed-input))
      (println (calculate-gears parsed-input))))
  )
