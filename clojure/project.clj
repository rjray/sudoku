(defproject solving-sudoku "0.1.0"
  :description "Sudoku solvers in Clojure"
  :license {:name "MIT"
            :url "https://opensource.org/license/mit/"}
  :dependencies [[org.clojure/clojure "1.12.5"]
                 [org.clojure/tools.cli "1.4.256"]]
  :plugins [[lein-kibit "0.1.11"]]
  :main sudoku.core
  :repl-options {:init-ns sudoku.core})
