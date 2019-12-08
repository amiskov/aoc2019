(ns aoc2019.day8
  (:require [clojure.string :as str]))

(def data (->> (slurp (str "resources/day8.txt"))
               (str/trim)))

(def digits (->> (str/split data #"")
                 (map #(Integer/parseInt %))))

(defn split-to-layers [w h digits]
  (let [pixels-in-layer (* w h)]
    (vec (partition pixels-in-layer digits))))

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

(defn overlay-2-layers [bottom top]
  (let [mix-colors (fn [b t]
                     (case t
                       2 b
                       1 1
                       0 0))]
    (mapv (fn [b t] (mix-colors b t)) bottom top)))

; List of layers -> Layer
; Layer is a list of digits
(defn process-layers [acc layers]
  (let [bottom (if (empty? acc) (peek layers) acc)
        top (peek (pop layers))]
    (if (nil? top)
      acc
      (process-layers (overlay-2-layers bottom top)
                      (pop layers)))))

(defn digit->colorbox [digit]
  (case digit
    0 "▓"
    1 "░"))

(defn digits->img [digits width height]
  (->> digits
       (split-to-layers width height)
       (process-layers [])
       (map digit->colorbox)
       (partition width)
       (map #(str/join %))))

(defn part2 [digits]
  (digits->img digits 25 6))

(part2 digits)

; ░▓▓░▓▓░░▓▓▓░░▓▓░▓▓░▓░▓▓░▓
; ░▓▓░▓░▓▓░▓░▓▓░▓░▓▓░▓░▓▓░▓
; ░▓▓░▓░▓▓▓▓░▓▓▓▓░▓▓░▓░░░░▓
; ░▓▓░▓░▓░░▓░▓▓▓▓░▓▓░▓░▓▓░▓
; ░▓▓░▓░▓▓░▓░▓▓░▓░▓▓░▓░▓▓░▓
; ▓░░▓▓▓░░░▓▓░░▓▓▓░░▓▓░▓▓░▓