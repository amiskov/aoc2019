(ns aoc2019.day2
  (:require [clojure.test :refer :all]))

(defn instruction? [pos intcode]
  (let [opcode (get intcode pos)]
    (or (= opcode 1) (= opcode 2))))

(defn do-op [op-pos program op]
  (let [a-pos (get program (+ 1 op-pos))
        b-pos (get program (+ 2 op-pos))
        store-pos (get program (+ 3 op-pos))]
    (assoc program store-pos (op (get program a-pos)
                                 (get program b-pos)))))

(defn calculate [pos program]
  (let [op (get program pos)]
    (cond
      (= op 1) (do-op pos program +)
      (= op 2) (do-op pos program *)
      :else (throw "Unknown operator"))))

(defn evaluate [program]
  (loop [pos 0
         data program]
    (if (= 99 (get data pos))
      data
      (if (instruction? pos data)
        (recur (+ pos 4) (calculate pos data))
        (recur (+ pos 1) data)))))

(defn update-noun-and-verb [data noun verb]
  (-> data
      (assoc 1 noun)
      (assoc 2 verb)))

;; Part 1
(defn part1 [data]
  (let [data-1201 (update-noun-and-verb data 12 2)]
    (get (evaluate data-1201) 0)))

;; Part 2
(defn find-pair
  [intcode sentinel]
  (loop [noun 0
         verb-start 0
         verb-end 99]
    (let [guess-verb (quot (+ verb-start verb-end) 2)
          ic (update-noun-and-verb intcode noun guess-verb)
          guess-res (get (evaluate ic) 0)]
      (cond
        (> noun 99) (throw (ex-info "There's no answer." {:noun noun :verb guess-verb :guess-res guess-res}))
        (= guess-res sentinel) {:noun noun :verb guess-verb}
        (>= guess-verb 99) (recur (inc noun) 0 99)
        (<= guess-verb 0) (recur (dec noun) 0 99)
        (> guess-res sentinel) (recur noun verb-start (dec guess-verb))
        (< guess-res sentinel) (recur noun (inc guess-verb) verb-end)))))

(defn part2 [data]
  (let [res (find-pair data 19690720)]
    (+ (* 100 (:noun res)) (:verb res))))
