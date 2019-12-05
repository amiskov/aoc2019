(ns aoc2019.day5
  (:require [aoc2019.utils :as u]))

(defn num->digits
  "Returns sequence of digits of n"
  [n]
  (->> (iterate #(quot % 10) n)
       (take-while pos?)
       (mapv #(mod % 10))
       rseq
       vec))

(defn pos->value [program pointer mode]
  (let [value-pos (program pointer)]
    (case mode
      0 (program value-pos)
      1 value-pos
      (str "Invalid mode " mode))))

(defn pad
  "'(2 2) -> [2 2 0 0]"
  [n coll val]
  (take n (concat coll (repeat val))))

(defn take-params [modes opcode-pos program]
  (let [a (pos->value program (+ 1 opcode-pos) (first modes))
        b (pos->value program (+ 2 opcode-pos) (second modes))
        result-pos (program (+ 3 opcode-pos))]
    [a b result-pos]))

(defn parse-opcode-value [ocv]
  (if (= 99 ocv)
    {:opcode ocv :param-modes '(0 0 0 0)}
    (let [digits (num->digits ocv)
          opcode (last digits) ; TODO: actually opcode has two digits
          params-modes (drop 2 (reverse digits))] ; store param modes left-to-right, like in program
      {:opcode opcode
       :param-modes (pad 4 params-modes 0)})))

(defn handle-1-and-2 [modes pos opcode-fn program]
  (let [[a b result-pos] (take-params modes pos program)]
    (assoc program result-pos (opcode-fn a b))))

(defn handle-3 [pos program input]
  (assoc program (program (inc pos)) input))

(defn handle-4 [modes pos program]
  (let [value (pos->value program (+ 1 pos) (first modes))]
    (prn "Output: " value)
    program))

(defn handle-5
  "Returns the next position"
  [modes pos program]
  (let [a (pos->value program (+ 1 pos) (first modes))
        b (pos->value program (+ 2 pos) (second modes))]
    (if (= a 0)
      (+ pos 3)
      b)))

(defn handle-6
  "Returns the next position"
  [modes pos program]
  (let [a (pos->value program (+ 1 pos) (first modes))
        b (pos->value program (+ 2 pos) (second modes))]
    (if (= a 0)
      b
      (+ 3 pos))))

(defn handle-7 [modes pos program]
  (let [a (pos->value program (+ 1 pos) (first modes))
        b (pos->value program (+ 2 pos) (second modes))
        res-pos (program (+ 3 pos))
        res-val (if (< a b) 1 0)]
    (assoc program res-pos res-val)))

(defn handle-8 [modes pos program]
  (let [a (pos->value program (+ 1 pos) (first modes))
        b (pos->value program (+ 2 pos) (second modes))
        res-pos (program (+ 3 pos))
        res-val (if (= a b) 1 0)]
    (assoc program res-pos res-val)))

(defn execute [program initial-input]
  (loop [cur-pos 0
         p program]
    (let [{opcode :opcode params-modes :param-modes} (parse-opcode-value (p cur-pos))]
      (cond
        (= opcode 99) p
        (= opcode 1) (recur (+ cur-pos 4) (handle-1-and-2 params-modes cur-pos + p))
        (= opcode 2) (recur (+ cur-pos 4) (handle-1-and-2 params-modes cur-pos * p))
        (= opcode 3) (recur (+ cur-pos 2) (handle-3 cur-pos p initial-input))
        (= opcode 4) (recur (+ cur-pos 2) (handle-4 params-modes cur-pos p))
        (= opcode 5) (recur (handle-5 params-modes cur-pos p) p)
        (= opcode 6) (recur (handle-6 params-modes cur-pos p) p)
        (= opcode 7) (recur (+ cur-pos 4) (handle-7 params-modes cur-pos p))
        (= opcode 8) (recur (+ cur-pos 4) (handle-8 params-modes cur-pos p))
        :else (str "Something went wrong " p "; " opcode "; " cur-pos)))))

(def data (u/file->intcode "day5.txt"))

; Must output 6731945 to console
(execute data 1)

; Must output 9571668 in console
(execute data 5)
