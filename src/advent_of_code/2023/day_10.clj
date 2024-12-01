(ns advent-of-code.2023.day-10
  (:use [advent-of-code.core]))

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
  "Returns the start position S as '(x y)"
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
  "Returns a list:
   first element is the ordered points following the main tube '((x1 y1) (x2 y2) ...)
   second element is a set with unordered points following the main tube #{(x3 y3) (x1 y1) ...}"
  ([i-map]
   (let [start-pos (find-start-position i-map)
         {directions \S} directions-rules
         next-step (get-next-step i-map directions start-pos start-pos)]
     (follow-the-tube i-map next-step (list start-pos))))
  ([i-map current steps]
   (let [[last-pos] steps
         [_ char pos] current
         {directions char} directions-rules
         next-step (get-next-step i-map directions pos last-pos)]
     (if (= (second next-step) \S)
       (list (conj steps pos) (set (conj steps pos)))
       (recur i-map next-step (conj steps pos)))))
  )

(defn between [p a b]
  (or (and (>= p a) (<= p b)) (and (<= p a) (>= p b))))

(defn ray-cast [point tubes]
    (loop [tubes tubes
           [ax ay] (last tubes)
           [bx by] (first tubes)
           [px py :as point] point
           inside false]
      (cond
        (nil? bx) inside
        (or (and (= px ax) (= py ay)) (and (= px bx) (= py by))) false
        (and (= ay by) (= py ay) (between px ax bx)) false
        (between py, ay, by)
        (if (or (and (= py ay) (>= by ay)) (and (= py by) (>= ay by)))
          (recur (rest tubes) (first tubes) (second tubes) point inside)
          (let [c (- (* (- ax px) (- by py)) (* (- bx px) (- ay py)))]
            (if (= c 0)
              false
              (if (= (< ay by) (> c 0))
                (recur (rest tubes) (first tubes) (second tubes) point (not inside))
                (recur (rest tubes) (first tubes) (second tubes) point inside)))
            ))
        :else (recur (rest tubes) (first tubes) (second tubes) point inside)
        )
      )
    )

(defn remove-and-get-garbage
  "Return a list:
      first element is a new map only with the main tube and the second are the remaining spaces are dots
      second element is a list of dots as '(x y)"
  ([i-map tubes]
   (remove-and-get-garbage i-map '(0 0) tubes []))
  ([i-map current polygon dot-list]
   (let [[x y] current]
     (if (>= y (alength i-map))
       dot-list
       (if (get-item current i-map)
         ;performance for checking in set was (as expected) way better than in list (40x faster)
         ;Elapsed time: 966.386704 msecs
         ;Elapsed time: 38781.53955 msecs
         (if (polygon current)
           (recur i-map (list (inc x) y) polygon dot-list)
           (recur i-map (list (inc x) y) polygon (conj dot-list current)))
         (recur i-map (list 0 (inc y)) polygon dot-list)))))
  )

(defn -main []
  (time
    (let [input (to-array-2d (get-lines-2023 "day-10.txt"))
          [tubes tubes-set] (follow-the-tube input)
          all-dots (remove-and-get-garbage input tubes-set)]
      (println (/ (count tubes-set) 2))                     ;part 1
      (println (count (filter true? (map-indexed #(ray-cast %2 tubes) all-dots)))) ;part 2
      ;it's possible to optimize the algorithm applying flood fill instead of checking every dot,
      ;but I am not going to do it
      )
    )
  )
