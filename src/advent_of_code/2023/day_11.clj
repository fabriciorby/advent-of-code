(ns advent-of-code.2023.day-11
  (:use [advent-of-code.core]))

(defn index-to-expand
  "Receives a matrix and returns a vector with indexes to expand"
  [i-map]
  (loop [acc []
         [line & remaining] i-map
         i 0]
    (if (not-empty line)
      (if (every? #{\.} (set line))
        (recur (conj acc i) remaining (inc i))
        (recur acc remaining (inc i)))
      acc))
  )

(defn calculate-distance [[ax ay] [bx by]]
  (+ (- (max ax bx) (min ax bx)) (- (max ay by) (min ay by))))

(defn calculate-all
  ([points]
   (calculate-all points 0))
  ([[first & tail] i]
    (if (not-empty tail)
      (recur tail (reduce #(+ %1 (calculate-distance first %2)) i tail))
      i))
  )

(defn expand-points
  "Receives a list of points, two list of indexes to expand (x and y), and a number of expansion
  Returns a list with the expanded points"
  [points expand-x expand-y n]
  (loop [[[x y :as point] & remaining] points
         expanded-points []]
    (if (nil? point)
      expanded-points
      (let [expanded (list (* n (count (filter #(< % x) expand-x))) (* n (count (filter #(< % y) expand-y))))]
        (recur remaining (conj expanded-points (map + point expanded)))))
    )
  )

(defn -main []
  (let [input (get-lines-2023 "day-11.txt")
        i-map (to-array-2d input)
        expand-y (index-to-expand i-map)
        expand-x (index-to-expand (transpose i-map))
        galaxies (map second (reverse (find-items i-map #(= \# %))))]
    (println (calculate-all (expand-points galaxies expand-x expand-y (dec 2))))
    (println (calculate-all (expand-points galaxies expand-x expand-y (dec 1000000))))
    )
  )
