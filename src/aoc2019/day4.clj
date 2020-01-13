;; https://adventofcode.com/2019/day/4
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
  "Checks part 1 criteria (p must be 6 digits long)"
  [p]
  (loop [digits (num->digits p)
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

(defn password?-ext
  "Checks part 2 criteria (p must already satisfy part 1 criteria)"
  [p]
  (let [matches (re-seq #"(\d)\1+" (str p)) ; e.g (["333" "3"] ["22" "2"])
        digit-counts (map #(count (get % 0)) matches)] ; e.g. (3 2)
    (seq (filter #(= 2 %) digit-counts))))

(defn part1 [range]
  (count (filter password? range)))

(defn part2 [range]
  (count (filter password?-ext (filter password? range))))
