(ns sudoku.core
  (:require [sudoku.solver-dfs :as dfs]
            [clojure.tools.cli :refer [parse-opts]]))

;;; This is the harness for running the different Sudoku solvers against
;;; specified input files. It handles reading the files and timing the execution
;;; of each puzzle.

;; Like the core time macro, but rather than printing the elapsed time it
;; returns a list of (result, time). Returned value is in milliseconds.
(defmacro ^:private time-it [expr]
  `(let [start# (. System (nanoTime))
         ret#   ~expr
         end#   (/ (double (- (. System (nanoTime)) start#)) 1000000.0)]
     (list ret# end#)))
