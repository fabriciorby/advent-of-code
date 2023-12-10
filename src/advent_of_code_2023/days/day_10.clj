(ns advent-of-code-2023.days.day-10
  (:use [advent-of-code-2023.core]))

(def directions-rules
  {
   \. {:next '()}
   \| {:next-up     '(\| \F \7 \S) :next-down  '(\| \L \J \S)}
   \- {:next-right  '(\- \J \7 \S) :next-left  '(\- \L \F \S)}
   \L {:next-right  '(\7 \- \J \S) :next-up    '(\| \7 \F \S)}
   \J {:next-left   '(\F \- \L \S) :next-up    '(\| \7 \F \S)}
   \7 {:next-left   '(\F \- \L \S) :next-down  '(\L \| \J \S)}
   \F {:next-right  '(\J \- \7 \S) :next-down  '(\L \| \J \S)}
   \S {:next-up     '(\| \7 \F)   :next-down   '(\L \| \J)
       :next-right  '(\- \J \7)   :next-left   '(\- \L \F)}
   }
  )

(defn apply-direction [direction current]
  (apply (direction {:next-left  #(list (dec %1) %2)
                     :next-right #(list (inc %1) %2)
                     :next-down  #(list %1 (inc %2))
                     :next-up    #(list %1 (dec %2))
                     }) current)
  )

(defn get-item [current i-map]
  (let [[x y] current
        n-rows (alength i-map)
        n-cols (alength (aget i-map 0))]
    (if (and (>= x 0) (>= y 0) (<= y (dec n-rows)) (<= x (dec n-cols)))
      (aget i-map y x) nil)))


(defn find-start-position
  ([i-map]
   (find-start-position i-map '(0 0)))
  ([i-map current]
   (let [[x y] current]
     (if-let [item (get-item current i-map)]
        (if (= item \S)
          current
          (find-start-position i-map (list (inc x) y)))
        (find-start-position i-map (list 0 (inc y))))))
  )

(defn get-direction-coordinates [directions position]
  (map #(list %1 (apply-direction %1 position)) (keys directions))
  )

(defn get-next-step [i-map directions pos last-pos]
  (let [surroundings (map #(list (first %) (get-item (second %) i-map) (second %)) (get-direction-coordinates directions pos))
        valid-steps (filter #(some #{(second %)} ((first %) directions)) surroundings)
        [next-step] (filter #(not= (last %) last-pos) valid-steps)]
    next-step
    )
  )

(defn follow-the-tube
  ([i-map]
   (let [start-pos (find-start-position i-map)
         {directions \S} directions-rules
         next-step (get-next-step i-map directions start-pos start-pos)]
     (follow-the-tube i-map next-step (list start-pos))
     )
   )
  ([i-map current steps]
   (let [[last-pos] steps
         [_ char pos] current
         {directions char} directions-rules
         next-step (get-next-step i-map directions pos last-pos)]
     (if (= (second next-step) \S)
       (conj steps pos)
       (recur i-map next-step (conj steps pos))
       )
     )
   )
  )

(defn -main []
  (let [input (to-array-2d (get-lines "day-10.txt"))]
    (/ (count (follow-the-tube input)) 2)
    )
  )
