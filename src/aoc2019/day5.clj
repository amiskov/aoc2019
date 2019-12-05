(ns aoc2019.day5
  (:require [clojure.string :as str]))


(defn to-data [input]
  (mapv #(Integer/parseInt %)
        (-> input
            (str/trim)
            (str/split #","))))

(defn num->digits
  "Returns sequence of digits of n"
  [n]
  (->> n
       (iterate #(quot % 10))
       (take-while pos?)
       (mapv #(mod % 10))
       rseq))

(defn code-by-pos [program param-pos param-mode]
  (if (= param-mode 1) ; immediate mode, position is the value
    (get program param-pos)
    (let [param-pointer (get program param-pos)]
      (get program param-pointer))))

(= (code-by-pos [1002,4,3,4,33] 1 1) 4)
(= (code-by-pos [1002,4,3,4,33] 1 0) 33)
(code-by-pos [3 3 1107 8 8 3 4 3 99] 4 1)

(defn pad [n coll val]
  (take n (concat coll (repeat val))))

(defn parse-opcode-value [ocv]
  ; (prn "opcode value" ocv)
  (if (= 99 ocv)
    {:opcode ocv :param-modes 0}
    (let [digits (num->digits ocv)
          opcode (first  (take-last 1 digits)) ; TODO: must take 2
          params-modes (drop 2 (reverse digits))] ; args modes left-to-right, like in program
      {:digits digits
       :opcode opcode
       :param-modes (pad 4 params-modes 0)})))

(defn handle-1-and-2 [pm pos opcode-fn program]
  (let [a (code-by-pos program (+ 1 pos) (first pm))
        b (code-by-pos program (+ 2 pos) (second pm))
        result-pos (get program (+ 3 pos))]
    ; (prn "Summing: " a " + " b " store to " result-pos)
    (assoc program result-pos (opcode-fn a b))))

(defn handle-3 [pm pos program input] ; Opcode 3
  ; (prn pm pos input)
  (assoc program (get program (inc pos)) input))

(defn handle-4 [pm pos program] ; Opcode 4
  (let [value (code-by-pos program (+ 1 pos) (first pm))]
    (prn "Output: " value)
    program))

(defn handle-5
  "Returns the next position (not program)"
  [pm pos program] ; jump-if-true
  (let [a (code-by-pos program (+ 1 pos) (first pm))
        b (code-by-pos program (+ 2 pos) (second pm))]
    (if (= a 0)
      (+ pos 3)
      b)))

(defn handle-6
  "Returns the next position (not program)"
  [pm pos program] ; jump-if-true
  (let [a (code-by-pos program (+ 1 pos) (first pm))
        b (code-by-pos program (+ 2 pos) (second pm))]
    (if (= a 0)
      b
      (+ 3 pos))))

(defn handle-7 [pm pos program]
  (let [a (code-by-pos program (+ 1 pos) (first pm))
        b (code-by-pos program (+ 2 pos) (second pm))
        res-pos (get program (+ 3 pos))
        res-val (if (< a b) 1 0)]
    ; (prn "from 7:" "a:" a "b:"b "res-pos val:" res-pos res-val)
    (assoc program res-pos res-val)))

(defn handle-8 [pm pos program]
  (let [a (code-by-pos program (+ 1 pos) (first pm))
        b (code-by-pos program (+ 2 pos) (second pm))
        res-pos (get program (+ 3 pos))
        res-val (if (= a b) 1 0)]
    (assoc program res-pos res-val)))

(defn execute [program initial-input]
  (loop [cur-pos 0
         p program]
    (let [{opcode :opcode params-modes :param-modes} (parse-opcode-value (get p cur-pos))]
      ; (prn (mapv (fn [a b] (str "i"a ":" b)) (range 0 47) p))
      ; (prn "opcode:" opcode "params modes:" params-modes)
      (cond
        (= opcode 99) p
        (= opcode 1) (recur (+ cur-pos 4) (handle-1-and-2 params-modes cur-pos + p))
        (= opcode 2) (recur (+ cur-pos 4) (handle-1-and-2 params-modes cur-pos * p))
        (= opcode 3) (recur (+ cur-pos 2) (handle-3 params-modes cur-pos p initial-input))
        (= opcode 4) (recur (+ cur-pos 2) (handle-4 params-modes cur-pos p))
        (= opcode 5) (recur (handle-5 params-modes cur-pos p) p)
        (= opcode 6) (recur (handle-6 params-modes cur-pos p) p)
        (= opcode 7) (recur (+ cur-pos 4) (handle-7 params-modes cur-pos p))
        (= opcode 8) (recur (+ cur-pos 4) (handle-8 params-modes cur-pos p))
        :else (str "Something went wrong " p "; " opcode "; " cur-pos)))))

(def input "3,225,1,225,6,6,1100,1,238,225,104,0,101,14,135,224,101,-69,224,224,4,224,1002,223,8,223,101,3,224,224,1,224,223,223,102,90,169,224,1001,224,-4590,224,4,224,1002,223,8,223,1001,224,1,224,1,224,223,223,1102,90,45,224,1001,224,-4050,224,4,224,102,8,223,223,101,5,224,224,1,224,223,223,1001,144,32,224,101,-72,224,224,4,224,102,8,223,223,101,3,224,224,1,223,224,223,1102,36,93,225,1101,88,52,225,1002,102,38,224,101,-3534,224,224,4,224,102,8,223,223,101,4,224,224,1,223,224,223,1102,15,57,225,1102,55,49,225,1102,11,33,225,1101,56,40,225,1,131,105,224,101,-103,224,224,4,224,102,8,223,223,1001,224,2,224,1,224,223,223,1102,51,39,225,1101,45,90,225,2,173,139,224,101,-495,224,224,4,224,1002,223,8,223,1001,224,5,224,1,223,224,223,1101,68,86,224,1001,224,-154,224,4,224,102,8,223,223,1001,224,1,224,1,224,223,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,108,226,677,224,1002,223,2,223,1006,224,329,1001,223,1,223,1007,226,226,224,1002,223,2,223,1006,224,344,101,1,223,223,1008,226,226,224,102,2,223,223,1006,224,359,1001,223,1,223,107,226,677,224,1002,223,2,223,1005,224,374,101,1,223,223,1107,677,226,224,102,2,223,223,1006,224,389,101,1,223,223,108,677,677,224,102,2,223,223,1006,224,404,1001,223,1,223,1108,677,226,224,102,2,223,223,1005,224,419,101,1,223,223,1007,677,226,224,1002,223,2,223,1006,224,434,101,1,223,223,1107,226,226,224,1002,223,2,223,1006,224,449,101,1,223,223,8,677,226,224,102,2,223,223,1006,224,464,1001,223,1,223,1107,226,677,224,102,2,223,223,1005,224,479,1001,223,1,223,1007,677,677,224,102,2,223,223,1005,224,494,1001,223,1,223,1108,677,677,224,102,2,223,223,1006,224,509,101,1,223,223,1008,677,677,224,102,2,223,223,1005,224,524,1001,223,1,223,107,226,226,224,1002,223,2,223,1005,224,539,101,1,223,223,7,226,226,224,102,2,223,223,1005,224,554,101,1,223,223,1108,226,677,224,1002,223,2,223,1006,224,569,1001,223,1,223,107,677,677,224,102,2,223,223,1005,224,584,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,599,101,1,223,223,108,226,226,224,1002,223,2,223,1005,224,614,101,1,223,223,1008,677,226,224,1002,223,2,223,1005,224,629,1001,223,1,223,7,226,677,224,102,2,223,223,1005,224,644,101,1,223,223,8,677,677,224,102,2,223,223,1005,224,659,1001,223,1,223,8,226,677,224,102,2,223,223,1006,224,674,1001,223,1,223,4,223,99,226")

; Must output 6731945 to console
(execute (to-data input) 1)

; 9571668 in console
(execute (to-data input) 5)

; position mode, input is equal to 8: output 1 (if it is) or 0 (if it is not).
(execute (to-data "3,9,8,9,10,9,4,9,99,-1,8") 8)

; position mode, input is less than 8: output 1 (if it is) or 0 (if it is not).
(execute (to-data "3,9,7,9,10,9,4,9,99,-1,8") 8)

; OK: immediate mode, input is equal to 8: output 1 (if it is) or 0 (if it is not).
(execute (to-data "3,3,1108,-1,8,3,4,3,99") 8)

; OK: jump: output 0 if the input was zero or 1 if the input was non-zero
(execute (to-data "3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9") 0)

(execute (to-data "3,3,1105,-1,9,1101,0,0,12,4,12,99,1") 1)

; OK: input is less than 8; output 1 (if it is) or 0 (if it is not).
(execute (to-data "3,3,1107,-1,8,3,4,3,99") 8)


; larger example

(def prg [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99])

(execute prg 7)




(defn update-noun-and-verb [data noun verb]
  (-> data
      (assoc 1 noun)
      (assoc 2 verb)))

(defn find-pair [program sentinel]
  (loop [noun 0
         verb-start 0
         verb-end 99]
    (let [guess-verb (quot (+ verb-start verb-end) 2) ; binary search
          p (update-noun-and-verb program noun guess-verb)
          guess-result (get (execute p) 0)]
      (cond
        (= guess-result sentinel) {:noun noun :verb guess-verb}
        (>= guess-verb 99) (recur (inc noun) 0 99)
        (> guess-result sentinel) (recur noun verb-start (dec guess-verb))
        (< guess-result sentinel) (recur noun (inc guess-verb) verb-end)))))


(def data ; vector of digits
  (mapv #(Integer/parseInt %)
        (-> "resources/day2.txt"
            (slurp)
            (str/trim)
            (str/split #","))))


(defn part1 [data]
  (-> data
      (update-noun-and-verb 12 2)
      execute
      (get 0)))

(defn part2 [data]
  (let [pair (find-pair data 19690720)]
    (+ (* 100 (:noun pair)) (:verb pair))))

(= (part1 data) 3058646)

(= (part2 data) 8976)

