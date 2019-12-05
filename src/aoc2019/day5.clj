(ns aoc2019.day5)

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

(defn take-2-params [modes opcode-pos program]
  (let [a (pos->value program (+ 1 opcode-pos) (first modes))
        b (pos->value program (+ 2 opcode-pos) (second modes))]
    [a b]))

(defn take-3-params [modes opcode-pos program]
  (let [result-pos (program (+ 3 opcode-pos))]
    (conj (take-2-params modes opcode-pos program) result-pos)))

(defn parse-opcode-value [ocv]
  (if (= 99 ocv)
    {:opcode ocv :param-modes '(0 0 0 0)}
    (let [digits (num->digits ocv)
          opcode (last digits) ; TODO: actually opcode has two digits
          params-modes (drop 2 (reverse digits))] ; store modes in left-to-right order
      {:opcode opcode
       :param-modes (pad 4 params-modes 0)})))

(defmulti evaluate (fn [{pos :pos prg :program}]
                     (:opcode (parse-opcode-value (prg pos)))))

(defmethod evaluate 1 [{pos :pos prg :program}]
  (let [{modes :param-modes} (parse-opcode-value (prg pos))
        [a b res-pos] (take-3-params modes pos prg)
        res-val (+ a b)]
    {:program (assoc prg res-pos res-val) :pos (+ 4 pos)}))

(defmethod evaluate 2 [{pos :pos prg :program}]
  (let [{modes :param-modes} (parse-opcode-value (prg pos))
        [a b res-pos] (take-3-params modes pos prg)
        res-val (* a b)]
    {:program (assoc prg res-pos res-val) :pos (+ 4 pos)}))

(defmethod evaluate 3 [{pos :pos prg :program input :input}]
  {:program (assoc prg (prg (inc pos)) input) :pos (+ 2 pos)})

(defmethod evaluate 4 [{pos :pos prg :program out :out}]
  (let [{modes :param-modes} (parse-opcode-value (prg pos))
        a (pos->value prg (+ 1 pos) (first modes))]
    {:out (conj out a) :pos (+ 2 pos)}))

(defmethod evaluate 5 [{pos :pos prg :program}]
  (let [{modes :param-modes} (parse-opcode-value (prg pos))
        [a b] (take-2-params modes pos prg)
        res-pos (if (= a 0) (+ pos 3) b)]
    {:pos res-pos}))

(defmethod evaluate 6 [{pos :pos prg :program}]
  (let [{modes :param-modes} (parse-opcode-value (prg pos))
        [a b] (take-2-params modes pos prg)
        res-pos (if (= a 0) b (+ 3 pos))]
    {:pos res-pos}))

(defmethod evaluate 7 [{pos :pos prg :program}]
  (let [{modes :param-modes} (parse-opcode-value (prg pos))
        [a b res-pos] (take-3-params modes pos prg)
        res-val (if (< a b) 1 0)]
    {:program (assoc prg res-pos res-val) :pos (+ 4 pos)}))

(defmethod evaluate 8 [{pos :pos prg :program}]
  (let [{modes :param-modes} (parse-opcode-value (prg pos))
        [a b res-pos] (take-3-params modes pos prg)
        res-val (if (= a b) 1 0)]
    {:program (assoc prg res-pos res-val) :pos (+ 4 pos)}))

(defmethod evaluate 99 [_] {:halt true})

(defn execute [data]
  (let [upd (evaluate data)]
    (if (:halt upd)
      (:out data)
      (execute (conj data upd)))))