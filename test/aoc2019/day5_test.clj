(ns aoc2019.day5-test
  (:require [clojure.test :refer :all]
            [aoc2019.utils :as u]
            [aoc2019.day5 :as d]))

(def data (u/file->intcode "day5.txt"))

(deftest part1-test
  (testing ""
    (is (= (last (d/execute {:pos 0 :program data :input 1 :halt false :out []}))
           6731945))))

(deftest part2-test
  (testing ""
    (is (= (last (d/execute {:pos 0 :program data :input 5 :out []}))
           9571668))))

(deftest pos-to-value-test
  (testing ""
    (is (= (d/pos->val [1002 4 3 4 33] 1 1) 4))
    (is (= (d/pos->val [1002 4 3 4 33] 1 0) 33))
    (is (= (d/pos->val [3 3 1107 8 8 3 4 3 99] 4 1) 8))))