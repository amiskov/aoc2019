(ns aoc2019.day4)

(defn num->digits
  "Returns sequence of digits of n"
  [n]
  (->> n
       (iterate #(quot % 10))
       (take-while pos?)
       (mapv #(mod % 10))
       rseq))

(defn password?
  "Checks password criteria. n must be 6 digits long."
  [n]
  (loop [digits (num->digits n)
         prev nil
         has-adjacent false]
    (if (empty? digits)
      has-adjacent
      (let [cur (first digits)]
        (cond
          (nil? prev) (recur (rest digits) cur has-adjacent)
          (< cur prev) false
          (= cur prev) (recur (rest digits) cur true)
          :else (recur (rest digits) cur has-adjacent))))))

(defn password?-ext [n]
  (let [matches (re-seq #"(\d)\1+" (str n)) ; e.g (["333" "3"] ["22" "2"])
        digit-counts (map #(count (get % 0)) matches)] ; e.g. (3 2)
    (seq (filter #(= 2 %) digit-counts))))

(defn part1 [range]
  (count (filter password? range)))

(defn part2 [range]
  (count (filter password?-ext (filter password? range))))
