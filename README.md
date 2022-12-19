# Advent of Code Solutions

Folder structure is such that when in the day's directory (e.g. `./2018/1/`) the solutions, when available, can be run using the following commands:

### Swift

``` bash
swift swift/main.swift $(cat input.txt)
```

### Clojure

``` bash
clj clojure/core.clj $(cat input.txt)
```

In 2022, I switched to using babashka (though clj will probably still work), and since whitespace became significant I started slurping the input file instead.

```bash
bb clojure/core.clj input.txt
```
