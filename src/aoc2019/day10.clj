(ns aoc2019.day10
  (:require
   [clojure.string :as str]
   [clojure.set :as set]))

(defn get-width [field]
  (count (first field)))

(defn get-height [field]
  (count field))

(defn asteroid? [[x y] field]
  (= ((field y) x) "#"))

(defn find-all-coords
  "Matrix with every possible coordinate of the field"
  [width height]
  (-> (for [xs (range 0 width)
            ys (range 0 height)]
        [ys xs]) ; [[x1 y1] [x2 y1] ...]
      vec))

; Vector -> Set
(defn find-all-asteroids [field]
  (set (filter #(asteroid? % field)
               (find-all-coords (get-width field)
                                (get-height field)))))

; filename -> set of asteroids
(defn file->asteroids [file]
  (find-all-asteroids
   (vec (map #(str/split % #"")
             (str/split
              (->> (str "resources/" file)
                   slurp)
              #"\n")))))

(defn get-x-row [x-rng y]
  (-> (for [xs x-rng]
        [xs y])
      vec))

(defn get-y-row [y-rng x]
  (-> (for [ys y-rng]
        [x ys])
      vec))

(defn get-perimeter-coords
  "Returns a set of equadistant coordinates for a given point."
  [[x y] distance]
  (let [x-range (range (- x distance) (inc (+ x distance)))
        y-range (range (- y distance) (inc (+ y distance)))
        x-row-above (get-x-row x-range (- y distance))
        x-row-below (get-x-row x-range (+ y distance))
        y-row-left (get-y-row y-range (- x distance))
        y-row-right (get-y-row y-range (+ x distance))]
    (->> [x-row-above x-row-below y-row-left y-row-right]
         (apply concat)
         set)))

(defn find-asteroids-in-perimeter [pov distance asteroids]
  (set/intersection (get-perimeter-coords pov distance)
                    asteroids))

(defn dot-on-line? [[x-pov y-pov] [x-obs y-obs] [x-ast y-ast]]
  (zero? (- (* (- x-ast x-pov) (- y-obs y-pov))
            (* (- y-ast y-pov) (- x-obs x-pov)))))

(defn dot-between? [[x-pov y-pov] [x-obs y-obs] [x-ast y-ast]]
  (or (and (<= x-pov x-obs x-ast)
           (<= y-pov y-obs y-ast))
      (and (>= x-pov x-obs x-ast)
           (>= y-pov y-obs y-ast))
      (and (<= x-pov x-obs x-ast)
           (>= y-pov y-obs y-ast))
      (and (>= x-pov x-obs x-ast)
           (<= y-pov y-obs y-ast))))

(defn obstacle-between?
  "Returns true if dot [x-obs y-obs] is between other coords."
  [[x-pov y-pov] [x-obs y-obs] [x-ast y-ast]]
  (and (dot-on-line? [x-pov y-pov] [x-obs y-obs] [x-ast y-ast])
       (dot-between? [x-pov y-pov] [x-obs y-obs] [x-ast y-ast])))

(defn visible?
  "Returns true if there is no obstacles between pov and asteroid"
  [pov asteroid obstacles]
  (not-any? #(obstacle-between? pov % asteroid)
            obstacles))

(defn find-visible-for [pov asteroids]
  (loop [other-asteroids (set/difference asteroids #{pov})
         step 1
         saved #{}]
    (if (empty? other-asteroids)
      saved
      (let [asteroids-in-perimeter (find-asteroids-in-perimeter pov step asteroids)
            visible (set (filter #(visible? pov % saved) asteroids-in-perimeter))]
        (recur
         (set/difference other-asteroids asteroids-in-perimeter)
         (inc step)
         (set/union saved visible))))))

(defn make-visibility-map [asteroids]
  (loop [povs asteroids
         acc {}]
    (if (empty? povs)
      (sort-by val > acc)
      (let [pov (first povs)
            visible (find-visible-for pov asteroids)]
        (recur (rest povs)
               (assoc acc pov (count visible)))))))

(make-visibility-map (file->asteroids "day10_3-4_8.txt"))
(make-visibility-map (file->asteroids "day10_5-8_33.txt"))
(make-visibility-map (file->asteroids "day10_1-2_35.txt"))
(make-visibility-map (file->asteroids "day10_6-3_41.txt"))
(make-visibility-map (file->asteroids "day10_11-13_210.txt"))

; part 1
(make-visibility-map (file->asteroids "day10.txt"))
