(ns aoc2019.day3
  (:require [clojure.test :refer :all]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn str->step [s]
  {:dir (get s 0)
   :len (Integer/parseInt (subs s 1))})

(defn str->line [s]
  (mapv str->step (str/split s #",")))

(defn ranges->coords [x-range y-range]
  (-> (for [xs x-range
            ys y-range]
        [xs ys])
      vec))

(defmulti go (fn [dir & _] dir))

(defmethod go \R [_ x1 y1 steps]
  (let [x2 (+ x1 steps)]
    (ranges->coords (range (inc x1) (inc x2))
                    [y1])))

(defmethod go \L [_ x1 y1 steps]
  (let [x2 (- x1 steps)]
    (ranges->coords (range (dec x1) (dec x2) -1)
                    [y1])))

(defmethod go \U [_ x1 y1 steps]
  (let [y2 (+ y1 steps)]
    (ranges->coords [x1]
                    (range (inc y1) (inc y2)))))

(defmethod go \D [_ x1 y1 steps]
  (let [y2 (- y1 steps)]
    (ranges->coords [x1]
                    (range (dec y1) (dec y2) -1))))

(defn all-coords [line-str]
  (loop [steps (str->line line-str)
         acc []
         point {:x 0 :y 0}]
    (if (empty? steps)
      acc
      (let [f (first steps)
            step-coords (go (:dir f) (:x point) (:y point) (:len f))
            last-point (last step-coords)]
        (recur (rest steps)
               (into [] (concat acc step-coords))
               {:x (first last-point) :y (last last-point)})))))

(defn find-equal-coords [c1 c2]
  (let [s1 (set c1)
        s2 (set c2)]
    (set/intersection s1 s2)))

(defn sum-coords [coord-vect]
  (+ (Math/abs (get coord-vect 0))
     (Math/abs (get coord-vect 1))))

(defn part1 [line-str1 line-str2]
  (let [coord-intersection (find-equal-coords (all-coords line-str1)
                                              (all-coords line-str2))]
    (apply min (mapv sum-coords coord-intersection))))

; part2
(defn steps-to-point [line-coords p]
  (loop [coords line-coords
         c 0]
    (if (empty? coords)
      c
      (if (= (first coords) p)
        (inc c)
        (recur (rest coords) (inc c))))))

(defn steps-to-point-for-both-lines [l1 l2 p]
  [(steps-to-point l1 p) (steps-to-point l2 p)])

(defn part2 [line-str1 line-str2]
  (let [l1-coords (all-coords line-str1)
        l2-coords (all-coords line-str2)
        equal-coords (find-equal-coords l1-coords l2-coords)
        paths (mapv #(steps-to-point-for-both-lines l1-coords l2-coords %) equal-coords)]
    (apply min (mapv #(apply + %) paths))))