# Day 1

## Naming
Clojure (and other lisps) allows using all kinds of symbols in naming. So, functions like this:

```clojure
(defn calculate-fuel [mass] ...)
(defn calculate-total-fuel [mass] ...)
```

Could be named like this:

```clojure
(defn mass->fuel [mass] ...)
(defn mass->fuel+fuel [mass] ...)
```

## Abstraction
It's better to create more general abstractions and then compose them into more concrete with piping (e.g. thread-first/last operators).

## Laziness
Recursive functions like this:

```clojure
(defn mass->fuel+fuel [mass]
  (loop [fuel (mass->fuel mass)
         acc 0]
    (if (<= fuel 0)
      acc
      (recur (mass->fuel fuel) (+ acc fuel)))))
```

Could be written with lazy sequence like this:

```clojure
(defn mass->fuel+fuel [mass]
  (->> (iterate mass->fuel mass)
       rest ; skip module mass, only fuel counts
       (take-while pos?)
       (reduce +)))
```

## Standard Library
- [iterate](https://clojuredocs.org/clojure.core/iterate)
- [take-while](https://clojuredocs.org/clojure.core/take-while)
