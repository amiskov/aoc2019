# Vector get value
These forms are equivalent:

```clojure
(get program pos)
(program pos)
```

# Case
There's `case`!

```clojure
(case param-mode
    0 (memory value)
    1 value
    (str "Invalid param-mode " param-mode)))
```
