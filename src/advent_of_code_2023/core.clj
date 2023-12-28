(ns advent-of-code-2023.core
  (:require [clojure.java.io :as io]))

(comment "Parse helpers")

(defn get-lines [filename]
  (with-open [reader (io/reader (io/resource (str "inputs/" filename)))]
    (doall (line-seq reader)))
  )

(defn split-with-exclusive [pred coll]
  [(take-while pred coll) (rest (drop-while pred coll))])

(comment "Matrix Helpers Below")

(defn get-item
  "Returns a item in a matrix, if out of boundaries returns nil."
  [current i-map]
  (let [[x y] current
        n-rows (alength i-map)
        n-cols (alength (aget i-map 0))]
    (if (and (>= x 0) (>= y 0) (<= y (dec n-rows)) (<= x (dec n-cols)))
      (aget i-map y x) nil)))

(defn find-item
  "Returns the position of an item in a matrix as '(x y)"
  ([i-map pred]
   (find-item i-map '(0 0) pred))
  ([i-map current pred]
   (let [[x y] current]
     (if (> y (alength i-map))
       nil
       (if-let [item (get-item current i-map)]
         (if (pred item)
           (list item current)
           (recur i-map (list (inc x) y) pred))
         (recur i-map (list 0 (inc y)) pred)))))
  )

(defn find-items
  "Returns a list of positions of items in a matrix"
  ([i-map pred]
   (find-items i-map () (find-item i-map '(0 0) pred) pred)
   )
  ([i-map acc item pred]
   (let [[_ [x y] :as item] item]
     (if (nil? item)
       acc
       (recur i-map (conj acc item) (find-item i-map (list (inc x) y) pred) pred)
       )
     )
   )
  )

(defn transpose [m]
  (apply mapv vector m))

(comment "Collection Helpers")

(defn in?
  "Returns true if collection contains element"
  [coll element]
  (some #(= element %) coll))