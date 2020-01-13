;; https://adventofcode.com/2019/day/1
(ns aoc2019.day1)

(defn mass->fuel [mass]
  (- (quot mass 3) 2))

(defn mass->fuel+fuel [mass]
  (->> (iterate mass->fuel mass)
       rest ; only fuel counts
       (take-while pos?)
       (reduce +)))

(defn part1
  [data]
  (->> data
       (map mass->fuel)
       (reduce +)))

(defn part2
  [data]
  (->> (map mass->fuel+fuel data)
       (reduce +)))
