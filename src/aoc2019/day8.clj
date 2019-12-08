(ns aoc2019.day8
  (:require [clojure.string :as str]))

(def data (->> (slurp (str "resources/day8.txt"))
               (str/trim)))

(def digits (->> (str/split data #"")
                 (map #(Integer/parseInt %))))

(defn split-to-layers [w h digits]
  (let [pixels-in-layer (* w h)]
    (partition pixels-in-layer digits)))

(defn count-zeroes-for-layers [digits]
  {:count-of-zeroes (count (filter zero? digits))
   :digits digits})

(defn part1 [digits]
  (let [puzzle-layers (split-to-layers 25 6 digits)
        zero-count-maps (map count-zeroes-for-layers puzzle-layers)
        least-zeroes-layer-digits (:digits (first  (sort-by :count-of-zeroes zero-count-maps)))]
    (* (count  (filter #(= 1 %) least-zeroes-layer-digits))
       (count  (filter #(= 2 %) least-zeroes-layer-digits)))))

(= (part1 digits) 1560)
