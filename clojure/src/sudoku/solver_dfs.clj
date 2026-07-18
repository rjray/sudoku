(ns sudoku.solver-dfs
  (:require [clojure.set :as set]))

;;; Extracted from my solution to Project Euler problem 96.
;;;
;;; This is based strongly on Peter Norvig's essay on solving Sudoku puzzles,
;;; https://norvig.com/sudoku.html

;; Structural constants that will be used for each solution.

(defn- cross
  "Cross product of elements in A with elements in B"
  [A B]
  (for [a A, b B] (str a b)))

(def ^:private rows "ABCDEFGHI")
(def ^:private cols "123456789")
(def ^:private digits (set cols))
(def ^:private squares (cross rows cols))
(def ^:private unitlist (map set (concat (for [c cols] (cross rows (list c)))
                                         (for [r rows] (cross (list r) cols))
                                         (for [rs (partition 3 rows)
                                               cs (partition 3 cols)]
                                           (cross rs cs)))))
(def ^:private units (into {} (map (fn [s]
                                     (hash-map s (filter #(% s) unitlist)))
                                   squares)))
(def ^:private peers
  (into {} (map #(hash-map % (disj (apply set/union (units %)) %)) squares)))

;; These functions are the mechanics of the solution process.

(declare assign eliminate) ; Needed for reduce-by-*, below

(defn- reduce-by-square
  "Perform elimination based on the square"
  [values s]
  ;; We need the (current) values in square `s`, and in case there's only one
  ;; we need it as well.
  (cond
    (zero? (count (values s))) nil
    (= 1 (count (values s)))   (let [d2 (first (values s))]
                                 (reduce (fn [vs s2]
                                           (eliminate vs s2 d2))
                                         values (peers s)))
    :else                      values))

(defn- reduce-by-unit
  "Perform elimination based on the square's units"
  [values s d]
  (reduce (fn [vs u]
            (if (nil? vs)
              (reduced nil)
              (let [dplaces (filter #((values %) d) u)]
                (cond
                  (zero? (count dplaces)) nil
                  (= 1 (count dplaces))   (assign vs (first dplaces) d)
                  :else                   vs))))
          values (units s)))

(defn- eliminate
  "Eliminate `d` from `(values s)`, propagate when values or places < 2."
  [values s d]
  (if (or (nil? values)
          (not ((values s) d)))
    ;; Already eliminated or nil, just return `values`.
    values
    (-> (update values s disj d)
        (reduce-by-square s)
        (reduce-by-unit s d))))

(defn- assign
  "Eliminate all the other values (except `d`) from `(values s)` and propagate."
  [values s d]
  (let [others (disj (values s) d)]
    (reduce (fn [vs d2]
              (if (nil? vs)
                (reduced nil)
                (eliminate vs s d2)))
            values others)))

(defn- grid-values
  "Convert the string representation of the grid into a map"
  [grid]
  (into {} (map #(vector %1 %2) squares grid)))

(defn- parse-grid
  "Convert the given grid to a map of possible values ({square: set}), or
  throw if a contradiction is detected."
  [grid]
  (let [values (into {} (map #(hash-map % digits) squares))]
    (reduce (fn [vs [s d]]
              (if (not (digits d))
                vs
                (assign vs s d)))
            values (grid-values grid))))

(defn- solved?
  "Predicate: Determine if the passed-in values-map is a complete solution"
  [values]
  (every? #(= 1 %) (map count (vals values))))

(defn- search
  "Using depth-first search and propagation, try all possible values"
  [values]
  (if (solved? values)
    true
    (let [remaining (map #(list (count (values %)) %)
                         (filter #(> (count (values %)) 1) squares))
          [_ s]     (first (sort-by first remaining))]
      (some #(when-not (nil? %) %)
            (map #(search (assign values s %)) (values s))))))

(defn solve
  "Solve one puzzle, returning either `true` or `false` based on whether a
  solution was found"
  [puzzle]
  (search (parse-grid puzzle)))
