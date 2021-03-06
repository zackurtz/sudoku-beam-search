(defproject sudoku-beam-search "1.0"
  :description "Sodoku Solver using Beam Search"
  :license {:name "MIT License (MIT)"}
  :dependencies [[org.clojure/clojure "1.6.0"] [org.clojure/math.numeric-tower "0.0.4"]]
  :main ^:skip-aot sudoku-beam-search.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
