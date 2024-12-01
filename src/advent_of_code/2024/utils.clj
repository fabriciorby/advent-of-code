(ns advent-of-code.2024.utils
  (:require [clojure.java.io :as io]))

(comment "Parse helpers")

(defn get-lines [filename]
  (with-open [reader (io/reader (io/resource (str "inputs/" filename)))]
    (doall (line-seq reader)))
  )