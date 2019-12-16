(ns aoc2019.day5-test
  (:require [clojure.test :refer :all]
            [aoc2019.utils :as u]
            [aoc2019.day5 :as d]))

(def data (u/file->intcode "day5.txt"))

(deftest part1-test
  (testing "part 1 answer"
    (is (= (last (:out (d/execute {:address 0 :program data :input 1 :halt false :out []})))
           6731945))))

(deftest part2-test
  (testing "part 2 answer"
    (is (= (last (:out  (d/execute {:address 0 :program data :input 5 :out []})))
           9571668))))
