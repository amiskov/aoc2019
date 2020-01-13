;; https://adventofcode.com/2019/day/7
(ns aoc2019.day7
  (:require [clojure.math.combinatorics :as combo]
            [aoc2019.utils :as u]))

(defn num->digits [n]
  (->> (iterate #(quot % 10) n)
       (take-while pos?)
       (mapv #(mod % 10))
       rseq))

(defn pos->val [prg pos mode]
  (let [pos-for-check (prg pos)]
    (case mode
      0 (prg pos-for-check)
      1 pos-for-check
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

(defn take-params [qty pos prg]
  (-> (fn [i] (let [shift (inc i)
                    mode (nth (:modes (parse-code (prg pos))) i)]
                (if (= shift 3) (prg (+ pos shift))
                    (pos->val prg (+ pos shift) mode))))
      (mapv (range 0 qty))))

(defmulti evaluate (fn [{pos :pos prg :program}]
                     (:opcode (parse-code (prg pos)))))

(defmethod evaluate 1 [{pos :pos prg :program}]
  (let [[a b c] (take-params 3 pos prg)]
    {:program (assoc prg c (+ a b))
     :pos (+ 4 pos)}))

(defmethod evaluate 2 [{pos :pos prg :program}]
  (let [[a b c] (take-params 3 pos prg)]
    (prn "2|" a b c)
    {:program (assoc prg c (* a b))
     :pos (+ 4 pos)}))

(defmethod evaluate 3 [{pos :pos prg :program input :input}]
  (prn "3| pos:" pos "input:" input)
  (if (empty? input)
    {:pause true
     :program prg
     :input []
     :pos pos}

    {:program (assoc prg (prg (inc pos)) (first input))
     :pause false
     :input (rest input)
     :pos (+ 2 pos)}))

(defmethod evaluate 4 [{pos :pos prg :program out :out}]
  (prn "4| pos:" pos "out:" out)
  {:out (concat out (take-params 1 pos prg))
   :pos (+ 2 pos)})

(defmethod evaluate 5 [{pos :pos prg :program}]
  (let [[a b] (take-params 2 pos prg)
        next-pos (if (= a 0) (+ pos 3) b)]
    {:pos next-pos}))

(defmethod evaluate 6 [{pos :pos prg :program}]
  (let [[a b] (take-params 2 pos prg)
        next-pos (if (= a 0) b (+ 3 pos))]
    {:pos next-pos}))

(defmethod evaluate 7 [{pos :pos prg :program}]
  (let [[a b c] (take-params 3 pos prg)]
    {:program (assoc prg c (if (< a b) 1 0))
     :pos (+ 4 pos)}))

(defmethod evaluate 8 [{pos :pos prg :program}]
  (let [[a b c] (take-params 3 pos prg)]
    {:program (assoc prg c (if (= a b) 1 0))
     :pos (+ 4 pos)}))

(defmethod evaluate 99 [_] {:halt true})

;; data is a map: {:pos 0 :program intcode-program :input <INPUT-VALUE> :halt false :out []}
(defn execute [data]
  (loop [d data]
    (let [upd (evaluate d)
          new-data (conj d upd)]
      (cond
        (:halt new-data) new-data  ; Program finished. End.
        (:pause new-data) new-data ; Program reached 3 but input are empty. Pause it and pass output to the next AMP.
        :else (recur new-data)))))

(defn init-amplifier [intcode phase]
  {:pos 0
   :program intcode
   :phase phase ; for debug
   :input [phase]
   :out []
   :halt false
   :pause false})

(defn prepare-amps [intcode phases]
  (vec (map #(execute (init-amplifier intcode %)) phases)))

; new-input is a vector -> amp
(defn update-amp-input [amp new-input]
  (let [input (into [] (concat (:input amp) new-input))]
    (conj amp {:input input})))

(update-amp-input {:input [4] :out []} [3])

(defn run-amps [amps]
  (loop [amps amps
         amp-idx 0
         input [0]
         counter 0]
    (let [amp (update-amp-input (amps amp-idx) input)
          executed-amp (execute amp)
          {halt? :halt out :out} executed-amp]
      (if (and halt? (= 4 amp-idx)) ; halt amp-loop only if last amp is halted
        (last out)
        (let [next-input [(last out)]
              next-amp-idx (rem (inc amp-idx) (count amps))]
          (recur (assoc amps amp-idx executed-amp)
                 next-amp-idx
                 next-input
                 (inc counter)))))))


; ans: 139629729, [9,8,7,6,5]

;; Tests
(def p2t1 [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26, 27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5])
(run-amps (prepare-amps p2t1 [9,8,7,6,5]))
; ans: 139629729, [9,8,7,6,5]

(def p2t2 [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54, -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10])
(run-amps (prepare-amps p2t2 [9,7,8,5,6]))
; ans: 18216 [9,7,8,5,6]

(defn prepare-memory [intcode input phase]
  {:pos 0
   :halt false
   :program intcode
   :input [phase input]
   :out []})

(defn run-amps-once [intcode phases]
  (reduce
   (fn [input-from-prev-out phase]
     (last (:out
            (execute (prepare-memory intcode input-from-prev-out phase)))))
   0
   phases))

(defn part1 [intcode]
  (reduce max (map
               (fn [phases] (run-amps-once intcode phases))
               (combo/permutations [0 1 2 3 4]))))

(defn part2 [intcode]
  (reduce max (map
               (fn [phases] (run-amps (prepare-amps intcode phases)))
               (combo/permutations [9 8 7 6 5]))))

;; Tests
(= (part1  [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]) 43210)
(= (part1 [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]) 54321)
(= (part1 [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]) 65210)

(def puzzle-intcode (u/file->intcode "day7.txt"))

(= (part1 puzzle-intcode) 116680)
(= (part2 puzzle-intcode) 89603079)



