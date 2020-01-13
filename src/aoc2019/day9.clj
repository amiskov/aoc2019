;; https://adventofcode.com/2019/day/9
(ns aoc2019.day9
  (:require [aoc2019.utils :as u]))

(defn num->digits [n]
  (->> (iterate #(quot % 10) n)
       (take-while pos?)
       (mapv #(mod % 10))
       rseq))

(defn pos->val [prg pos mode rb]
  (let [pos-for-check (prg pos)]
    (case mode
      0 (prg pos-for-check)
      1 pos-for-check
      2 (prg (+ rb pos-for-check))
      (str "Invalid mode " mode))))

(defn pad
  "'(2 2) -> [2 2 0 0]"
  [n coll val]
  (take n (concat coll (repeat val))))

(defn parse-code [code]
  (if (= 99 code)
    {:opcode code :modes '(0 0 0 0)}
    (let [digits (num->digits code)
          opcode (last digits) ; TODO: actually opcode has two digits
          modes (drop 2 (reverse digits))] ; store modes in left-to-right order
      {:opcode opcode
       :modes (pad 4 modes 0)})))

(defn take-params [qty pos prg rb]
  (let [mapper (fn [i] (let [shift (inc i)
                             mode (nth (:modes (parse-code (prg pos))) i)]
                         (if (= shift 3) ; 3rd param
                           (if (= 2 mode) ; mode 2: program[pos + shift] + rb
                             (+ (prg (+ pos shift)) rb)
                             (prg (+ pos shift))) ; otherwise program[pos + shift]
                           (pos->val prg (+ pos shift) mode rb))))]
    (mapv mapper (range 0 qty))))

(defmulti evaluate (fn [{pos :pos prg :program}]
                     (:opcode (parse-code (prg pos)))))

(defmethod evaluate 1 [{pos :pos prg :program rb :relative-base}]
  (let [[a b c] (take-params 3 pos prg rb)]
    {:program (assoc prg c (+ a b))
     :pos (+ 4 pos)}))

(defmethod evaluate 2 [{pos :pos prg :program rb :relative-base}]
  (let [[a b c] (take-params 3 pos prg rb)]
    {:program (assoc prg c (* a b))
     :pos (+ 4 pos)}))

(defmethod evaluate 3 [{pos :pos prg :program input :input rb :relative-base}]
  (let [a (prg (inc pos))
        param-mode (first (:modes  (parse-code (prg pos))))
        pos-to-save-input (if (= 2 param-mode) (+ rb a) a)]
    {:program (assoc prg pos-to-save-input (first input))
     :input (rest input)
     :pos (+ 2 pos)}))

(defmethod evaluate 4 [{pos :pos prg :program out :out rb :relative-base}]
  (let [[a] (take-params 1 pos prg rb)
        new-out (concat out [a])]
    {:out new-out
     :pos (+ 2 pos)}))

(defmethod evaluate 5 [{pos :pos prg :program rb :relative-base}]
  (let [[a b] (take-params 2 pos prg rb)
        next-pos (if (not (= a 0)) b (+ pos 3))]
    {:pos next-pos}))

(defmethod evaluate 6 [{pos :pos prg :program rb :relative-base}]
  (let [[a b] (take-params 2 pos prg rb)
        next-pos (if (= a 0) b (+ 3 pos))]
    {:pos next-pos}))

(defmethod evaluate 7 [{pos :pos prg :program rb :relative-base}]
  (let [[a b c] (take-params 3 pos prg rb)]
    {:program (assoc prg c (if (< a b) 1 0))
     :pos (+ 4 pos)}))

(defmethod evaluate 8 [{pos :pos prg :program rb :relative-base}]
  (let [[a b c] (take-params 3 pos prg rb)]
    {:program (assoc prg c (if (= a b) 1 0))
     :pos (+ 4 pos)}))

(defmethod evaluate 9 [{pos :pos prg :program rb :relative-base}]
  (let [[a] (take-params 1 pos prg rb)]
    {:relative-base (+ rb a)
     :program prg
     :pos (+ 2 pos)}))

(defmethod evaluate 99 [_] {:halt true})

;; data is a map:
;; {:pos 0 :relative-base 0 :program intcode-program :input <INPUT-VALUE> :halt false :out []}
(defn execute [data]
  (loop [data data]
    (let [upd (evaluate data)]
      (if (:halt data)
        (conj data upd)
        (recur (conj data upd))))))

; Fast and durty: prepare large memory with many zeroes
(defn init-program [program limit]
  (into [] (concat
            program
            (apply vector-of :int (repeat limit 0)))))

(defn run-program [intcode input]
  (execute {:pos 0
            :relative-base 0
            :program (init-program intcode 1000)
            :input input
            :halt false
            :out []}))

; Part 1
(= (-> (u/file->intcode "day9.txt")
       (run-program [1])
       :out
       last) 2682107844)

; Part 2
(= (-> (u/file->intcode "day9.txt")
       (run-program [2])
       :out
       last) 34738)