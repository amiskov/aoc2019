(ns aoc2019.day5
  (:require [aoc2019.utils :as u]))

(defn num->digits
  "Converts number into list of digits"
  [n]
  (->> (iterate #(quot % 10) n)
       (take-while pos?)
       (mapv #(mod % 10))
       rseq))

(defn pad
  "Add val to coll from the right: '(2 2) -> [2 2 0 0]"
  [n coll val]
  (->> (repeat val)
       (concat coll)
       (take n)))

(defn parse-instruction-head
  "Returns a map with opcode and modes for it's parameters from the given intocode.
   Modes is a list of 4 digits for parameters, left-to-right order."
  {:test #(assert (= (parse-instruction-head 102)
                     {:opcode 2 :modes '(1 0 0 0)}))}
  [ic]
  (if (= ic 99)
    {:opcode ic :modes '()}
    (let [digits (num->digits ic)
          opcode (rem ic 100)
          modes (drop 2 (reverse digits))] ; modes in left-to-right order
      {:opcode opcode
       :modes (pad 4 modes 0)})))

(defn take-param
  "Returns parameter value according it's mode. Not for address to store a value."
  {:test #(do (assert (= (take-param 1 0 0 [1002, 2, 4, 0]) 4))
              (assert (= (take-param 1 0 1 [1102, 2, 4, 0]) 2)))}
  [shift address mode prg]
  (let [param (prg (+ address shift))]
    (case mode
      0 (prg param)
      1 param)))

(defn binary-op
  [{:keys [address program] :as memory} modes binary-fn]
  (let [a (take-param 1 address (first modes) program)
        b (take-param 2 address (second modes) program)
        addr (program (+ address 3))]
    (assoc memory
           :program (assoc program addr (binary-fn a b))
           :address (+ 4 address))))

(defn input-op
  [{:keys [address program input] :as memory}]
  (assoc memory
         :program (assoc program (program (inc address)) input)
         :address (+ 2 address)))

(defn output-op
  [{:keys [address program out] :as memory} modes]
  (assoc memory
         :out (concat out [(take-param 1 address (first modes) program)])
         :address (+ 2 address)))

(defn jump-op
  [{:keys [address program] :as memory} modes next-addr-fn]
  (let [a (take-param 1 address (first modes) program)
        b (take-param 2 address (second modes) program)]
    (assoc memory
           :address (next-addr-fn a b))))

(defn evaluate
  [{:keys [program address] :as memory}]
  (let [{oc :opcode modes :modes} (parse-instruction-head (program address))]
    (case oc
      1 (binary-op memory modes +)
      2 (binary-op memory modes *)
      3 (input-op memory)
      4 (output-op memory modes)
      5 (jump-op memory modes #(if (= %1 0) (+ address 3) %2))
      6 (jump-op memory modes #(if (= %1 0) %2 (+ address 3)))
      7 (binary-op memory modes #(if (< %1 %2) 1 0))
      8 (binary-op memory modes #(if (= %1 %2) 1 0))
      99 (assoc memory :halt true))))

(defn execute [memory]
  (loop [m memory]
    (if (:halt m)
      m
      (recur (evaluate m)))))

(def puzzle-input (u/file->intcode "day5.txt"))

; part 1
(:out (execute {:address 0 :program puzzle-input :input 1 :halt false :out []}))
; 6731945

; part 2
(:out (execute {:address 0 :program puzzle-input :input 5 :out []}))
; 9571668