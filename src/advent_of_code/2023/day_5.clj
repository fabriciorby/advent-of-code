(ns advent-of-code.2023.day-5
  (:require [advent-of-code.utils :as helper]
            [clojure.string :as str]))

(defn split-by-space-and-parse [line]
  (map #(Long/parseLong %) (str/split line #"\s+"))
  )

(defn get-map [type lines]
  (map split-by-space-and-parse (take-while #(not= "" %) (rest (drop-while #(not (str/includes? % type)) lines))))
  )

(defn parsed-input [lines]
  {
   :seeds (split-by-space-and-parse (last (str/split (first lines) #": ")))
   :conversions (list (get-map "soil" lines)
                 (get-map "fertilizer" lines)
                 (get-map "water" lines)
                 (get-map "light" lines)
                 (get-map "temperature" lines)
                 (get-map "humidity" lines)
                 (get-map "location" lines))
   })

(defn get-destination [line] (first line))

(defn get-source [line] (second line))

(defn get-range [line] (last line))

(defn get-new-destination [seed line]
  (if (and (>= (+ (get-range line) (get-source line)) seed) (>= seed (get-source line)))
    (reduced (- (+ seed (get-destination line)) (get-source line)))
    seed)
  )

(defn convert [seed line]
  (reduce get-new-destination seed line
  ))

(defn flatten-tuples [unflat-list]
  (if (seq? (first unflat-list))
    (partition 2 (flatten unflat-list))
    unflat-list)
  )

(defn get-min-destination [seed-range conversions]
  (let [seed (first seed-range)
        range (last seed-range)
        line (first conversions)]
    (if (not-empty line)
      ;está totalmente abaixo ou acima do range, então não tem que fazer nada
      (if (or (< (+ seed range) (get-source line)) (> seed (+ (get-range line) (get-source line))))
        (flatten-tuples (get-min-destination seed-range (rest conversions))) ;nao calculado
        ;mapeia tudo se não quebrar a range no meio
        (if (and (>= (+ (get-range line) (get-source line)) (+ seed range)) (>= seed (get-source line)))
          (list (- (+ seed (get-destination line)) (get-source line)) range) ;calculado
          ;quebrou pra cima e pra baixo
          (if (and (< seed (get-source line)) (> (+ range seed) (+ (get-source line) (get-range line))))
            (flatten-tuples (conj ()
                                  (list (get-destination line) (get-range line)) ;calculado
                                  (get-min-destination (list (+ (get-source line) (get-range line)) (- (+ range seed) (+ (get-source line) (get-range line)))) (rest conversions)) ;nao calculado
                                  (get-min-destination (list seed (- (get-source line) seed)) (rest conversions)) ;nao calculado
                                  ))
            ;quebrou a range por baixo
            (if (< seed (get-source line))
              (flatten-tuples (conj ()
                                    (list (get-destination line) (- (+ seed range) (get-source line))) ;calculado
                                    (get-min-destination (list seed (- (get-source line) seed)) (rest conversions)) ;nao calculado
                                    ))
              ;quebrou a range por cima
              (flatten-tuples (conj ()
                                    (list (- (+ seed (get-destination line)) (get-source line)) (- (+ (get-source line) (get-range line)) seed)) ; calculado
                                    (get-min-destination (list (+ (get-range line) (get-source line)) (- (+ seed range) (+ (get-source line) (get-range line)))) (rest conversions)) ;nao calculado
                                    ))
              )
            )
          )
        )
      seed-range
      )
    )
  )

(defn convert-to-min [seed-range conversions]
  (if (not-empty conversions)
    (if (seq? (first seed-range))
      (flatten-tuples (map #(convert-to-min (get-min-destination % (first conversions)) (rest conversions)) seed-range))
      (convert-to-min (get-min-destination seed-range (first conversions)) (rest conversions)))
    seed-range
    )
  )

(defn -main []
  (let [lines (helper/get-lines "2023/day-5.txt")
        parsed-input (parsed-input lines)
        seeds (get parsed-input :seeds)
        seeds-v2 (partition 2 seeds)
        ]
    (println (apply min (map #(reduce convert %1 (get parsed-input :conversions)) seeds)))
    (println (apply min (take-nth 2 (flatten (map #(convert-to-min % (get parsed-input :conversions)) seeds-v2)))))
    )
  )
