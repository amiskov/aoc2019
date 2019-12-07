(ns aoc2019.day5)

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
    {:program (assoc prg c (* a b))
     :pos (+ 4 pos)}))

(defmethod evaluate 3 [{pos :pos prg :program input :input}]
  {:program (assoc prg (prg (inc pos)) input)
   :pos (+ 2 pos)})

(defmethod evaluate 4 [{pos :pos prg :program out :out}]
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
  (let [upd (evaluate data)]
    (if (:halt upd)
      (conj data upd)
      (execute (conj data upd)))))
