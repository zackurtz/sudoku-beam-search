(defproject sudoku-beam-search "1.0"
  :description "Sodoku Solver using Beam Search"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]]
  :main ^:skip-aot sudoku-beam-search.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
