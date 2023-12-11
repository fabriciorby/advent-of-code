(ns advent-of-code-2023.days.day-11
  (:use [advent-of-code-2023.core]))

(defn expand [i-map]
  (loop [expanded-map []
         [line & remaining] i-map]
    (if (empty? line)
      expanded-map
      (if (every? #{\.} (set line))
        (recur (conj expanded-map line line) remaining)
        (recur (conj expanded-map line) remaining))))
  )

(defn transpose [m]
  (apply mapv vector m))

(defn enumerate-line [line i]
  (loop [i i
         [char & others] line
         new-line []]
    (if (nil? char)
      (list i new-line)
      (if (= char \#)
        (recur (inc i) others (conj new-line (inc i)))
        (recur i others (conj new-line char)))))
  )

(defn enumerate [i-map]
  (loop [map []
         [line & remaining] i-map
         i 0]
    (if (empty? line)
      map
      (let [[i new-line] (enumerate-line line i)]
        (recur (conj map new-line) remaining i)
        )
      ))
  )

(def transform (comp to-array-2d transpose expand transpose expand enumerate))

(defn calculate-distance [a b]
  (let [[ax ay] a
        [bx by] b]
    (+ (- (max ax bx) (min ax bx)) (- (max ay by) (min ay by)))
    )
  )

(defn calculate-all [points]
  (loop [[first & tail] (map second (reverse points))
         i 0]
    (if (empty? tail)
      i
      (let [acc
            (loop [[first-tail & remaining] tail
                   acc 0]
              (if (empty? first-tail)
                acc
                (recur remaining (+ acc (calculate-distance first first-tail))))
              )]
        (recur tail (+ i acc))))
    )
  )

(defn expand-2 [i-map]
  (loop [expanded-map []
         [line & remaining] i-map]
    (if (empty? line)
      expanded-map
      (if (every? #{\.} (set line))
        (recur (concat expanded-map (repeat 1000000 line)) remaining)
        (recur (conj expanded-map line) remaining))))
  )

(def transform-2 (comp to-array-2d transpose expand-2 transpose expand-2 enumerate))

(defn -main []
  (let [input (get-lines "day-11.txt")
        i-map (transform input)
        i-map-2 (transform-2 input)]
    (calculate-all (find-items i-map #(int? %)))
    (calculate-all (find-items i-map-2 #(int? %)))
    )
  )
