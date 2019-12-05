(ns aoc2019.day5
  (:require 
   [aoc2019.utils :as u]
   [clojure.tools.trace :as t]))
   


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
  (prn "ocv:" ocv)
  (if (= 99 ocv)
    {:opcode ocv :param-modes '(0 0 0 0)}
    (let [digits (num->digits ocv)
          opcode (last digits) ; TODO: actually opcode has two digits
          params-modes (drop 2 (reverse digits))] ; store param modes left-to-right, like in program
      {:opcode opcode
       :param-modes (pad 4 params-modes 0)})))

(defn handle-1 [data]
  (let [{pos :pos prg :program} data
        {modes :param-modes} (parse-opcode-value (prg pos))
        [a b res-pos] (take-params modes pos prg)
        res-val (+ a b)]
    (conj data {:program (assoc prg res-pos res-val)
                :pos (+ 4 pos)})))

(defn handle-2 [{pos :pos prg :program}]
  (let [{modes :param-modes} (parse-opcode-value (prg pos))
        [a b res-pos] (take-params modes pos prg)
        res-val (* a b)]
    {:program (assoc prg res-pos res-val) :pos (+ 4 pos)}))

(defn handle-3 [{pos :pos prg :program input :input}]
  {:program (assoc prg (prg (inc pos)) input) :pos (+ 2 pos)})

(defn handle-4 [{pos :pos prg :program out :out}]
  (let [{modes :param-modes} (parse-opcode-value (prg pos))
        a (pos->value prg (+ 1 pos) (first modes))]
    {:out (conj out a) :pos (+ 2 pos)}))

(defn handle-5 [{pos :pos prg :program}]
  (let [{modes :param-modes} (parse-opcode-value (prg pos))
        a (pos->value prg (+ 1 pos) (first modes))
        b (pos->value prg (+ 2 pos) (second modes))
        res-pos (if (= a 0) (+ pos 3) b)]
    {:pos res-pos}))

(defn handle-6 [{pos :pos prg :program}]
  (let [{modes :param-modes} (parse-opcode-value (prg pos))
        a (pos->value prg (+ 1 pos) (first modes))
        b (pos->value prg (+ 2 pos) (second modes))
        res-pos (if (= a 0) b (+ 3 pos))]
    {:pos res-pos}))

(defn handle-7 [{pos :pos prg :program}]
  (let [{modes :param-modes} (parse-opcode-value (prg pos))
        a (pos->value prg (+ 1 pos) (first modes))
        b (pos->value prg (+ 2 pos) (second modes))
        res-pos (prg (+ 3 pos))
        res-val (if (< a b) 1 0)]
    {:program (assoc prg res-pos res-val) :pos (+ 4 pos)}))

(defn handle-8 [{pos :pos prg :program}]
  (let [{modes :param-modes} (parse-opcode-value (prg pos))
        a (pos->value prg (+ 1 pos) (first modes))
        b (pos->value prg (+ 2 pos) (second modes))
        res-pos (prg (+ 3 pos))
        res-val (if (= a b) 1 0)]
    {:program (assoc prg res-pos res-val) :pos (+ 4 pos)}))

(defn opcode->opcode-fn [opcode]
  (case opcode
    1 handle-1
    2 handle-2
    3 handle-3
    4 handle-4
    5 handle-5
    6 handle-6
    7 handle-7
    8 handle-8
    (fn [n] (str "Something went wrong for opcode " opcode))))

(defn execute [data counter]
  (let [{pos :pos p :program} data
        {opcode :opcode} (parse-opcode-value (p pos))]
    (if (= 99 opcode)
      (:out data)
      (let [opcode-fn (opcode->opcode-fn opcode)]
        (execute (conj data (opcode-fn data)) (inc counter))))))

(def data (u/file->intcode "day5.txt"))

; Must output 6731945 to console
(execute {:pos 0 :program data :input 1 :out []} 0)

; Must output 9571668 in console
(execute {:pos 0 :program data :input 5 :out []} 0)
