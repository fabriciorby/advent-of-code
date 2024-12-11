(ns advent-of-code.2024.utils
  (:require [clojure.java.io :as io]))

(comment "Parse helpers")
(defn get-lines [filename]
  (with-open [reader (io/reader (str "resources/inputs/" filename))]
    (doall (line-seq reader)))
  )

(defn print-return
  "Prints and return value"
  ([item] (doto item prn)))

(defn split-with-exclusive [pred coll]
  [(take-while pred coll) (rest (drop-while pred coll))])

(comment "Matrix stuff")

(defn iterate-matrix
  "Receive an array-2d with a function that will receive m [i j v] as parameter"
  [f matrix] (map-indexed #(keep-indexed (fn [j v] (f matrix [% j v])) %2) matrix))

(defn to-matrix-i
  "Receives a list of lists and returns a indexed matrix:
  ([0 [[0 .] [1 .] [2 X] [3 .] [4 .] [5 .]]]
   [1 [[0 .] [1 S] [2 A] [3 M] [4 X] [5 .]]]
   [2 [[0 .] [1 A] [2 .] [3 .] [4 A] [5 .]]]
   [3 [[0 X] [1 M] [2 A] [3 S] [4 .] [5 S]]]
   [4 [[0 .] [1 X] [2 .] [3 .] [4 .] [5 .]]])"
  [input] (map-indexed vector (map #(map-indexed vector %) input)))

(defn get-item-i
  "Returns a item in a indexed matrix, if out of boundaries returns nil."
  [[x y] i-map]
  (let [n-rows (count i-map) n-cols (count (second (nth i-map 0)))]
    (if (and (>= x 0) (>= y 0) (<= y (dec n-rows)) (<= x (dec n-cols)))
      (second (nth (second (nth i-map x)) y)) nil)))

(defn get-item
  "Returns a item in a matrix, if out of boundaries returns nil."
  [i-map [x y]]
  (let [n-rows (alength i-map) n-cols (alength (aget i-map 0))]
    (if (and (>= x 0) (>= y 0) (<= y (dec n-rows)) (<= x (dec n-cols)))
      (aget i-map x y) nil)))

(defn get-item-details
  "Returns a item and it's coordinates, if out of boundaries returns nil."
  [i-map [x y]] [x y (get-item i-map [x y])])

(defn find-item
  "Returns the position of an item in a matrix as '(x y) or return nil"
  ([i-map pred]
   (find-item i-map '(0 0) pred))
  ([i-map current pred]
   (let [[x y] current]
     (if (> y (alength i-map))
       nil
       (if-let [item (get-item i-map current)]
         (if (pred item)
           (list item current)
           (recur i-map (list (inc x) y) pred))
         (recur i-map (list 0 (inc y)) pred))))))

(defn find-items
  "Returns a list of positions of items in a matrix"
  ([i-map pred] (find-items i-map () (find-item i-map '(0 0) pred) pred))
  ([i-map acc item pred]
   (let [[_ [x y] :as item] item]
     (if (nil? item) acc
                     (recur i-map (conj acc item) (find-item i-map (list (inc x) y) pred) pred)))))

(defn transpose [m]
  (apply mapv vector m))

(comment "Collection helper")

(defn in?
  "Returns true if collection contains element"
  [coll element]
  (some #(= element %) coll))