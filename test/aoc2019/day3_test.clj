(ns aoc2019.day3-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [aoc2019.day3 :as d]))

(def data ; 2 strings
  (mapv str
        (-> "resources/day3.txt"
            (slurp)
            (str/trim)
            (str/split #"\n"))))

(deftest part1-test
  (testing "check the mathattan distance to the intersection point closest to the central port"
    (is (= (d/part1 (get data 0) (get data 1))
           651))))

(deftest part2-test
  (testing "check fewest combined steps the wires must take to reach an intersection"
    (is (= (d/part2 (get data 0) (get data 1))
           7534))))
