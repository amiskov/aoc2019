(ns aoc2019.day1-test
  (:require [clojure.test :refer :all]
            [aoc2019.utils :as u]
            [aoc2019.day1 :as d]))

(def data (u/file->data "day1.txt" #(Integer/parseInt %)))

(deftest part1-test
  (testing "Fuel for all modules"
    (is (= (d/part1 data) 3178783))))

(deftest part2-test
  (testing "Fuel for all modules and their fuel"
    (is (= (d/part2 data) 4765294))))