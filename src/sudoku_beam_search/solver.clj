(ns sudoku-beam-search.solver
  (:require [clojure.math.numeric-tower :as math]))

(use 'clojure.set)

;;==============================================================
;; Sudoku Solver
(use 'sudoku-beam-search.utility)
;; All the test cases wrapped up in classes

(defn create-grid-info [block-rows block-cols]
  (let [edge-size (* block-rows block-cols)]
    {:row-count edge-size :col-count edge-size
      :blrow-size block-rows :blcol-size block-cols}))

(def info-4x4 (create-grid-info 2 2))
(def info-6x6 (create-grid-info 3 2))
(def info-9x9 (create-grid-info 3 3))

(def testc0 {:name 'test-easy :board easy-test :info info-4x4})
(def testc1 { :name 'test1 :board test1 :info info-4x4})
(def testc2 { :name 'test2 :board test2 :info info-4x4})
(def testc3 { :name 'test3 :board test3 :info info-4x4})
(def testc4 { :name 'test4 :board test4 :info info-4x4})
(def testc5 { :name 'test5 :board test5 :info info-6x6})
(def testc6 { :name 'test6 :board test6 :info info-6x6})
(def testc7 { :name 'test7 :board test7 :info info-9x9})
(def testc8 { :name 'test8 :board test8 :info info-9x9})
(def testc9 { :name 'test9 :board test9 :info info-9x9})

;; Limits of search

;; A function to generate the possible values for each cell
(defn gen-cell-values [grid-info]
  (set (range 1 (+ (* (:blrow-size grid-info) (:blcol-size grid-info)) 1))))

(defn block-start [grid-info row col]
  [(* (math/floor (/ row (:blrow-size grid-info))) (:blrow-size grid-info))
   (* (math/floor (/ col (:blcol-size grid-info))) (:blcol-size grid-info))])

(defn list-invalid [grid grid-info row col]
  (let [[box_row box_col] (block-start grid-info row col)]
    (union 
      (set (for [test_row (range 0 (:row-count grid-info)) :when (not (= test_row row))]
             (get-in grid [test_row col])))
      (set (for [test_col (range 0 (:col-count grid-info)) :when (not (= test_col col))]
             (get-in grid [row test_col])))
      (set (for [test_row (range box_row (+ box_row (:blrow-size grid-info)))
                 test_col (range box_col (+ box_col (:blcol-size grid-info)))
                 :when (not (and (= test_row row) (= test_col col)))]
                   (get-in grid [test_row test_col]))))))

;; Lists the possible values for the cell at (get-in grid [row col])
(defn list-valid [grid grid-info row col]
  (let [possible (gen-cell-values grid-info)]
    (difference possible (list-invalid grid grid-info row col))))

;; Genrates a list of all possible moves from a given grid
(defn find-moves [grid grid-info]
  (let [row-count (:row-count grid-info) col-count (:col-count grid-info) temp-valid nil]
    (for [row (range 0 row-count) col (range 0 col-count) 
          :when (empty-cell (get-in grid [row col]))]
        [[row col] (list-valid grid grid-info row col)])))


;; My thought is that the search can be designed to not explore invalid states,
;; and such I only have to check if the entire board is filled to check for goal
;; state
;; is-full is the goal test in my search
(defn is-full [grid]
  (let [row-count (numrows grid) col-count (numcols grid)]
    (= 0 
      (count (take 1 (for [row (range 0 row-count) col (range 0 col-count)
                           :when (empty-cell (get-in grid [row col]))]
                        'empty))))))

;; Generates new states based on a state and a move
(defn apply-move [grid move]
  (let [[row col] (move 0) values (move 1)]
    (for [value values]
        { :board (assoc grid row (assoc (grid row) col value))
          :siblings (count values)})))


;; Generates all new states from one state
(defn successor-states [grid grid-info]
  (let [moves (find-moves grid grid-info)]
    (apply concat (for [move moves]
        (apply-move grid move)))))

(defn wrap-successors [parent successors]
  (for [successor successors]
    (merge parent 
      { :board (:board successor) 
        :parent parent
        :depth (+ (:depth parent 0) 1)
        :siblings (:siblings successor) } )))

;; Add a list of games to the closed map
(defn update-closed [closed games]
  (loop [newclosed closed remaining games]
    (if (empty? remaining)
      newclosed
      (recur (assoc closed (:board (first remaining)) true) (rest remaining)))))


;; Generates all successor games from the given game, checking for and removing
;; duplicate states
;; Correctly sets the value of sibling-games
(defn expand-game-best [game closed]
  (let [not-in-closed? (fn [state] (not (get closed (:board state)))) 
        unwrapped (filter not-in-closed? (successor-states (:board game) (:info game)))
        expanded-games (wrap-successors game unwrapped)]
    [expanded-games (update-closed closed expanded-games)]))


;; Generates all successor games, ignoring duplicate states
;; correctly sets the value of sibling-games to the number
;; of other states that were generated to the same square
(defn expand-game [game]
  (let [boards (successor-states (:board game) (:info game))]
    (wrap-successors game boards)))

;; The main search function the arguements are:
;; game - Start state (has to be a game class)
;; q-fn - Queueing function (q-fn old new)
;; depthlimit - Depth limit (I never used it for sudoku)
;; reps_allowed - controls if duplicate state checking is on or off
(defn general-search [game q-fn depthlimit & [reps_allowed]]
  (let [closed (hash-map) 
        open (list game)
        new_nodes nil num_gen 0]
    (loop [open (list game) num_exp 1 closed {(:board game) true}] (do
      ;(println "open: " (count open) (type open))
      ;(println "closed: " (count closed))
      (let [to-expand (first open)]
        ;(if (= 0 (mod num_exp 1)) (println "to-expand: " (:board to-expand)))
        (cond
          (nil? (:board to-expand)) ["Failed: to-expand empty" to-expand]
          (is-full (:board to-expand)) [to-expand num_exp]
          (and depthlimit (> (:depth to-expand 0) depthlimit)) ["depth limit reached"]
          true
            (let [[new_nodes new_closed] (if reps_allowed
                              [(expand-game to-expand) {}]
                              (expand-game-best to-expand closed))]
              ;;(set num_gen (+ num_gen (count new_nodes)))
              (recur (q-fn (rest open) new_nodes) (+ num_exp 1) new_closed))))))))

;; DFS Queueing function, (removes nil states if they exist)
(defn q-dfs [old new]
  (doall (remove nil? (into old new))))
;; BFS Queueing function, (also removes any nil states)
(defn q-bfs [old new]
  (doall (remove nil? (into new old))))

;; Helper function for BESTFS
;; inserts a game into old-games maintaining ascending order of
;; (:siblings game). Recursive, suffers from stackoverflows.
(defn insert-game [game old-games]
  (cond
   (empty? old-games) (list game)
   (<= (:siblings game)
       (:siblings (first old-games))) (conj old-games game)
   true (concat (list (first old-games)) (insert-game game (rest old-games)))))



(defn sibling-compare [newstate oldstate]
  (<= (:siblings newstate 0) (:siblings oldstate 0)))

;; Another helper function for BESTFS
;; same as insert-game, except it doesn't fill up the stack for
;; large problems. (I also tried a tail recursive version, but the loop
;; was faster)
(defn insert-game-loop [new-game old-games]
  (if (empty? old-games)
    (list new-game)
    (let [num-old (count old-games)]
      (loop [first-half '() second-half old-games]
        (cond
          (empty? second-half) (concat first-half (list new-game))
          (<= (:siblings new-game) (:siblings (first second-half)))
            (concat first-half (list new-game) second-half)
          true (recur (concat first-half (list (first second-half))) (rest second-half)))))))

;; BESTFS Queueing function
;; Output is a list in ascending order of
;; Sibling games (The number of other 'choices' when the state was generated)
;; NOTE:
;; The 'best' order for sudoku is to expand based on:
;;  1. Depth (Deepest first)
;;  2. (If there is a tie in depth) Sibling-games (Least remaining values)
;; The function bases the ordering only on 2.
(defn q-bestfs [old new]
  (loop [sorted old current (first new) remaining (rest new)]
    (if (nil? current)
      sorted  
      (recur (insert-game-loop current sorted) (first remaining) (rest remaining)))))

;; BEAM search
;; This function consists of two ideas:
;;    1. As I discussed above my BESTFS isn't really (in my opinion) best, in
;;       this queueing function the deepest nodes really are aways expanded
;;       first, with the remaining value based ordering acting to break ties
;;
;;    2. The part that makes this a beam search. I discard states in the open
;;       list that either will never be expanded (the goal *has* to be found
;;       first) or cannot lead to a goal. Discarding these states prevents the
;;       open list from growing to the point where it becomes too slow to
;;       manage. The basic idea is this queueing function only keeps the
;;       states in new that were created by assigning a value to *one* square
;;       (the square with the least possible values remaining).
;;
(defn q-beam [old new]
  (let [new-sorted (sort-by :siblings new)
        new-subseq (take (max (:siblings (first new-sorted) 0) 1) new-sorted)]
    (q-bestfs old new-subseq)))

;; A wrapper to take time stats on the general-search function
(defn search-stats [game q-fs depthlimit & [reps_allowed]]
    (time (general-search game q-fs depthlimit reps_allowed)))
