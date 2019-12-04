(ns aoc2019.day4-test
  (:require [clojure.test :refer :all]
            [aoc2019.day4 :as d]))

(def begin 152085)
(def end 670283)

(deftest part1-test
  (testing "number of passwords according part 1 criteria"
    (is (= (d/part1 (range begin (inc end)))
           1764))))

(deftest part2-test
  (testing "number of passwords according part 2 criteria"
    (is (= (d/part2 (range begin (inc end)))
           1196))))