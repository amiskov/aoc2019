(ns aoc2019.day2)

(defn code-by-pos [program pos]
  (let [param-pointer (get program pos)]
    (get program param-pointer)))

(defn evaluate-instruction [opcode-pos opcode-fn program]
  (let [a (code-by-pos program (+ 1 opcode-pos))
        b (code-by-pos program (+ 2 opcode-pos))
        result-pos (get program (+ 3 opcode-pos))]
    (assoc program result-pos (opcode-fn a b))))

(defn execute [program]
  (loop [code-pos 0
         p program]
    (let [code (get p code-pos)]
      (cond
        (= code 99) p
        (= code 1) (recur (+ code-pos 4) (evaluate-instruction code-pos + p))
        (= code 2) (recur (+ code-pos 4) (evaluate-instruction code-pos * p))
        :else (recur (+ code-pos 1) p)))))

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

(defn part1 [data]
  (-> data
      (update-noun-and-verb 12 2)
      execute
      (get 0)))

(defn part2 [data]
  (let [pair (find-pair data 19690720)]
    (+ (* 100 (:noun pair)) (:verb pair))))
