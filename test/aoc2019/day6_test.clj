(ns aoc2019.day6-test
  (:require [clojure.test :refer :all]
            [ubergraph.core :as ug]
            [clojure.string :as str]
            [aoc2019.utils :as u]
            [aoc2019.day6 :as d]))

(def puzzle-graph
  (->> (fn [s] (str/split s #"\)"))
       (u/file->data "day6.txt")
       d/graph-data
       ug/graph))

(deftest part1-test
  (testing "part 1: checksum"
    (is (= (d/part1 puzzle-graph)
           314247))))

(deftest part2-test
  (testing "path from :YOU orbit to :SAN orbit"
    (is (= (d/part2 puzzle-graph)
           514))))
