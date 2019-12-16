(ns aoc2019.day5
  (:require [aoc2019.utils :as u]))

(defn num->digits
  "Converts number into list of digits"
  [n]
  (->> (iterate #(quot % 10) n)
       (take-while pos?)
       (mapv #(mod % 10))
       rseq))

(defn addr->val
  "Converts address into value according mode"
  [prg address mode]
  (let [addr-for-check (prg address)]
    (case mode
      0 (prg addr-for-check)
      1 addr-for-check
      (str "Invalid mode " mode))))

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

(test #'parse-instruction-head)

(defn take-param
  "Returns parameter value according it's mode"
  {:test #(do (assert (= (take-param 1 0 0 [1002, 2, 4, 0]) 4))
              (assert (= (take-param 1 0 1 [1102, 2, 4, 0]) 2)))}
  [shift oc-address mode prg]
  (if (= shift 3)
    (prg (+ oc-address shift))
    (addr->val prg (+ oc-address shift) mode)))

(test #'take-param)

(defn evaluate-binary-op
  [{:keys [address program] :as memory} binary-fn]
  (let [modes (:modes (parse-instruction-head (program address)))
        a (take-param 1 address (first modes) program)
        b (take-param 2 address (second modes) program)
        addr (program (+ address 3))]
    (assoc memory
           :program (assoc program addr (binary-fn a b))
           :address (+ 4 address))))

(defn evaluate-jump-op
  [{:keys [address program] :as memory} next-addr-fn]
  (let [modes (:modes (parse-instruction-head (program address)))
        a (take-param 1 address (first modes) program)
        b (take-param 2 address (second modes) program)]
    (assoc memory
           :address (next-addr-fn a b address))))

(defmulti evaluate
  (fn [{:keys [address program] :as memory}]
    (:opcode (parse-instruction-head (program address)))))

(defmethod evaluate 1
  [memory]
  (evaluate-binary-op memory +))

(defmethod evaluate 2
  [memory]
  (evaluate-binary-op memory *))

(defmethod evaluate 3
  [{:keys [address program input] :as memory}]
  (assoc memory
         :program (assoc program (program (inc address)) input)
         :address (+ 2 address)))

(defmethod evaluate 4
  [{:keys [address program out] :as memory}]
  (let [modes (:modes (parse-instruction-head (program address)))
        param (take-param 1 address (first modes) program)]
    (assoc memory
           :out (concat out [param])
           :address (+ 2 address))))

(defmethod evaluate 5
  [memory]
  (evaluate-jump-op memory
                    (fn [a b address] (if (= a 0) (+ address 3) b))))

(defmethod evaluate 6
  [memory]
  (evaluate-jump-op memory
                    (fn [a b address] (if (= a 0) b (+ 3 address)))))

(defmethod evaluate 7
  [memory]
  (evaluate-binary-op memory
                      (fn [a b] (if (< a b) 1 0))))

(defmethod evaluate 8
  [memory]
  (evaluate-binary-op memory
                      (fn [a b] (if (= a b) 1 0))))

(defmethod evaluate 99
  [memory]
  (assoc memory :halt true))

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