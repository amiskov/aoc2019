(ns aoc2019.utils
  (:require
   [clojure.string :as str]))

(defn file->data [file data-transformer]
  (->> (slurp (str "resources/" file))
       str/split-lines
       (map data-transformer)))
