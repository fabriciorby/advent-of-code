(ns advent-of-code-2023.core
  (:require [clojure.java.io :as io]))

(defn get-lines [filename]
  (with-open [reader (io/reader (io/resource (str "inputs/" filename)))]
    (doall (line-seq reader)))
  )
