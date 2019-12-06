(ns aoc2019.day6
  (:require
   [ubergraph.alg :as alg]))

(defn find-neighbours [vertex pairs]
  (loop [ps pairs
         neighbours []]
    (let [[v d] (mapv keyword (first ps))]
      (if (empty? ps)
        (vec neighbours)
        (if (= v vertex)
          (recur (rest ps) (concat neighbours [d]))
          (recur (rest ps) neighbours))))))

(defn graph-data [pairs]
  (loop [pairs pairs
         gd {}]
    (if (empty? pairs)
      gd
      (let [vertex (keyword (first (first pairs)))]
        (if (empty? (vertex gd))
          (recur (rest pairs) (assoc gd vertex (find-neighbours vertex pairs)))
          (recur (rest pairs) gd))))))

(defn part1 [g]
  (reduce (fn [acc p] (+ acc (:cost p)))
          0
          (doall (alg/shortest-path g {:traverse true :start-node :COM}))))

(defn part2 [g]
  (- (:cost (doall (alg/shortest-path g :SAN :YOU))) 2))
