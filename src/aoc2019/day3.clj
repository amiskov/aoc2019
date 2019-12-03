(ns aoc2019.day3
  (:require [clojure.test :refer :all]
            [clojure.set :as set]
            [clojure.string :as str]))

(defn str->step [s]
  {:dir (get s 0)
   :len (Integer/parseInt (subs s 1))})

(defn str->line [s]
  (mapv str->step (str/split s #",")))

(defn go-right [point steps]
  (let [x1 (:x point)
        y1 (:y point)
        x2 (+ x1 steps)]
    (for [xs (range (inc x1) (inc x2))
          ys [y1]]
      [xs ys])))

(defn go-left [point steps]
  (let [x1 (:x point)
        y1 (:y point)
        x2 (- x1 steps)]
    (for [xs (range (dec x1) (dec x2) -1)
          ys [y1]]
      [xs ys])))

(defn go-up [point steps]
  (let [x1 (:x point)
        y1 (:y point)
        y2 (+ y1 steps)]
    (for [xs [x1]
          ys (range (inc y1) (inc y2))]
      [xs ys])))

(defn go-down [point steps]
  (let [x1 (:x point)
        y1 (:y point)
        y2 (- y1 steps)]
    (for [xs [x1]
          ys (range (dec y1) (dec y2) -1)]
      [xs ys])))

(defn make-step [point {dir :dir len :len}]
  (-> (cond
        (= dir \U) (go-up point len)
        (= dir \D) (go-down point len)
        (= dir \R) (go-right point len)
        (= dir \L) (go-left point len))
      vec))

(defn all-coords [line-str]
  (loop [steps (str->line line-str)
         acc []
         point {:x 0 :y 0}]
    (if (empty? steps)
      acc
      (let [f (first steps)
            step-coords (make-step point f)
            last-point (last step-coords)]
        (recur (rest steps) (into [] (concat acc step-coords)) {:x (first last-point) :y (last last-point)})))))

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