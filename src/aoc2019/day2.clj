(ns aoc2019.day2
  (:require [clojure.test :refer :all]
            [clojure.string :as str]
            [aoc2019.utils :as u]))

(def data
  (->> (str/split (str/trim (slurp "resources/day2.txt")) #",")
       (map #(Integer/parseInt %))))

(def restored-data (assoc (assoc (vec data) 1 12) 2 2))

(defn is-operator [pos data]
  (let [val (get data pos)]
    (or (= val 1) (= val 2))))
(is (is-operator 2 [1 3 2 0]))
(is (is-operator 3 [0 3 2 1 10]))

(defn do-op [op-pos data op]
  (let [a-pos (get data (+ 1 op-pos))
        b-pos (get data (+ 2 op-pos))
        store-pos (get data (+ 3 op-pos))]
    (assoc data store-pos (op (get data a-pos) (get data b-pos)))))
(do-op 0 [1, 1, 1, 1] +)
(do-op 2 [1, 1, 2, 3, 4, 0] *)


(defn calculate [pos data]
  (let [op (get data pos)]
    (cond
      (= op 1) (do-op pos data +)
      (= op 2) (do-op pos data *)
      :else (throw "Unknown operator"))))

(defn process [pos data]
  (if (= 99 (get data pos))
    data
    (if (is-operator pos data)
      (process (+ pos 4) (calculate pos data))
      (process (+ pos 1) data))))

(is (= (process 0 [2,4,4,5, 99, 0]) [2 4 4 5 99 9801]))
(is (= (process 0 [1,1,1,4,99,5,6,0,99]) [30 1 1 4 2 5 6 0 99]))
(is (= (get (process 0 restored-data) 0) 3058646))