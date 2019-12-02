(ns aoc2019.day2-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [aoc2019.day2 :as d]))

(def data
  (->> (str/split (str/trim (slurp "resources/day2.txt")) #",")
       (map #(Integer/parseInt %))
       vec))

(deftest intcode-evaluation
  (testing "intcode evaluation"
    (is (= (d/evaluate [2,4,4,5, 99, 0]) [2 4 4 5 99 9801]))
    (is (= (d/evaluate [1,1,1,4,99,5,6,0,99]) [30 1 1 4 2 5 6 0 99]))))

(deftest part1-test
  (testing "check the first value in evaluated intcode"
    (is (= (d/part1 data) 3058646))))

(deftest part2-test
  (testing "check `100 * noun + verb` expression"
    (is (= (d/part2 data) 8976))))

