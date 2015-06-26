(ns sudoku-beam-search.solver
  (:require [clojure.math.numeric-tower :as math]))

;;==============================================================
;; Sudoku Solver
(require 'sudoku-beam-search.utility)
;; All the test cases wrapped up in classes

(def testc1 { :name 'test1 :board test1 })
(def testc2 { :name 'test2 :board test2 })
(def testc3 { :name 'test3 :board test3 })
(def testc4 { :name 'test4 :board test4 })
(def testc5 { :name 'test5 :board test5 })
(def testc6 { :name 'test6 :board test6 })
(def testc7 { :name 'test7 :board test7 })
(def testc8 { :name 'test8 :board test8 })
(def testc9 { :name 'test9 :board test9 })

;; Limits of search

(def expand_limit 200000)
(def max-runtime 30000000)


;; A quick function to change the sudoku dimensions
(defn reset-dims [x y]
  (do
    (set! XBLOCKS x)
    (set! YBLOCKS y)
    (set! MAXX (* XBLOCKS YBLOCKS))
    (set! MAXY (* XBLOCKS YBLOCKS))))

;; A function to generate the possible values for each cell
;; (I saw the one in sudoku-basics after I had already written this)
(defn gen-cell-values []
  (range 1 (+ (* XBLOCKS YBLOCKS) 1)))

;; Lists the possible values for the cell at (aref grid row col)
(defn list-valid [grid row col]
  (let [valid (gen-cell-values)
        box_row (* (math/floor (/ row YBLOCKS)) YBLOCKS)
        box_col (* (math/floor (/ col XBLOCKS)) XBLOCKS)]
    (for [test_row (range 0 (- MAXY 1)) :when (not (= test_row row))]
      (remove (get-in grid test_row col) valid))
    (for [test_col (range 0 (- MAXX 1)) :when (not (= test_col col))]
      (remove (get-in grid row test_col) valid))
    (for [test_row (range box_row (+ box_row (- YBLOCKS 1)))]
      (for [test_col (range box_col (+ box_col (- XBLOCKS 1)))]
        (if (not (and (= test_row row) (= test_col col)))
            (remove (get-in grid [test_row test_col]) valid))))
    valid))

;; Genrates a list of all possible moves from a given grid
(defn find-moves [grid]
  (let [dim (count grid) temp-valid nil]
    (for [row (range 0 (nth 0 dim)) col (range 0 (nth 1 dim)) :when (empty-cell (get-in grid [row col]))]
        (do
          (set temp-valid (list-valid grid row col))
          (if (nil? temp-valid) (return nil))
          (list (list row col) temp-valid))))))


;; My thought is that the search can be designed to not explore invalid states,
;; and such I only have to check if the entire board is filled to check for goal
;; state
;; is-full is the goal test in my search
(defn is-full [grid]
  (let ((dim (array-dimensions grid)) )
    (= 0 (count (take 1 (for [row (range 0 (nth 0 dim)) col (range 0 (nth 1 dim))
                  :when (empty-cell (get-in grid [row col]))]
      'empty))))))

;; Generates a new state based on a state and a move
(defn apply-move [grid move]
  (let ((row (car (car move))) (col (cadr (car move))) (values (cadr move))
    (temp nil))
    (for [value values] :collect
      (do
        (setf temp (COPY-ARRAY grid))
        (setf (aref temp row col) value)
        (list temp (length values))))))


;; Generates all new states from one state
(defn successor-states [grid]
  (let ((moves (find-moves grid)))
    (for [move moves] :append
        (apply-move grid move))))

;; Generates all successor games from the given game, checking for and removing
;; duplicate states
;; Correctly sets the value of sibling-games
(defn expand-game-best [game closed]
  (let ((boards (successor-states (game-board game))))
    (loop for board in boards collect
        (if (not (gethash (prin1-to-string (car board)) closed))
        (do
          (setf (gethash (prin1-to-string (car board)) closed) t)
               { :name (game-name game)
                 :board (car board) :parent game
                 :depth (+ (game-depth game) 1)
                 :siblings (cadr board) } ))))))

;; Generates all successor games, ignoring duplicate states
;; correctly sets the value of sibling-games to the number
;; of other states that were generated to the same square
(defn expand-game [game]
  (let ((boards (successor-states (game-board game))))
    (loop for board in boards collect
      { :name (game-name game)
        :board (car board) :parent game
        :depth (+ (game-depth game) 1)
        :siblings (cadr board) } ))))

;; The main search function the arguements are:
;; game - Start state (has to be a game class)
;; q-fn - Queueing function (q-fn old new)
;; depthlimit - Depth limit (I never used it for sudoku)
;; reps_allowed - controls if duplicate state checking is on or off
(defn general-search [game q-fn depthlimit & reps_allowed]
  (let ((closed (make-hash-table :test #'equal))
    (goal nil) (open (list game)) (to_expand nil) (num_exp 0)
    (new_nodes nil) (num_gen 0) (start-time (get-internal-run-time)))
    (while (and (not goal) open) do
      (setf to-expand (pop open))
      (incf num_exp)
      ;(print open)
      ;(print to-expand)
      (cond
       ((null to-expand) (return-from basic-search open))
       ((is-full (game-board to-expand)) (setf goal to-expand))
       ((and depthlimit (> (game-depth to-expand) depthlimit)) t)
       (reps_allowed (do
               (setf new_nodes (expand-game to-expand))
               (setf num_gen (+ num_gen (length new_nodes)))
               (setf open (funcall q-fn open new_nodes))))
       (t (do
        (setf new_nodes (expand-game-best to-expand closed))
        (setf num_gen (+ num_gen (length new_nodes)))
        (setf open (funcall q-fn open new_nodes)))))
      (if (> num_exp expand_limit) (setf goal "EXCEEDED MAX NODES"))
      (if (> (- (get-internal-run-time) start-time) max-runtime)
          (setf goal "EXCEEDED MAX TIME")))
    (println "NUM. EXPANDED")
    (println num_exp)
    (println "NUM. GENERATED")
    (println num_gen)
    goal))

;; BFS Queueing function, (removes nil states if they exist)
(defn q-bfs (old new)
  (remove-if-not (lambda (x) x)
         (append old new)))
;; DFS Queueing function, (also removes any nil states)
(defn q-dfs (old new)
  (remove-if-not (lambda (x) x)
         (append new old)))

;; Helper function for BESTFS
;; inserts a game into old-games maintaining ascending order of
;; (sibling-games i)
(defn insert-game (game old-games)
  (cond
   ((null old-games) (list game))
   ((<= (sibling-games game) (sibling-games (car old-games)))
    (cons game old-games))
   (t (cons (car old-games) (insert-game game (cdr old-games))))))


;; Another helper function for BESTFS
;; same as insert-game, except it doesn't fill up the stack for
;; large problems. (I also tried a tail recursive version, but the loop
;; was faster)
(defn insert-game-loop (new-game old-games)
  (let [old-game nil remaining old-games num-old (length old-games)]
    (if (null old-games) (return-from insert-game-loop (list new-game)))
    (for [i (range 0 num-old)]
      (setf old-game (car remaining))
      (if (<= (sibling-games new-game) (sibling-games old-game))
        (return-from insert-game-loop
          (append (butlast old-games (- num-old i))
          (cons new-game (last old-games (- num-old i))))))
      (setf remaining (cdr remaining)))
    (return-from insert-game-loop
      (append old-games (list new-game)))))

;; BESTFS Queueing function
;; Output is a list in ascending order of
;; Sibling games (The number of other 'choices' when the state was generated)
;; NOTE:
;; In my opinion a the 'best' order for sudoku is to expand based on:
;;  1. Depth (Deepest first)
;;  2. (If there is a tie in depth) Sibling-games (Least remaining values)
;; The function bases the ordering only on 2.
(defn q-bestfs (old new)
  (let ((sorted old))
    (for [game new]
      (if game
        (setf sorted (insert-game-loop game sorted))))
    sorted))

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
(defn q-beam (old new)
  (let ((sorted nil) (templist nil))
    (for [game new]
      (if game
        (do
        (setf templist (insert-game-loop game sorted))
        (setf sorted
        (subseq templist 0
        (min (length templist) (max (sibling-games (car templist)) 1)))))))
    (append sorted old)))

;; A wrapper to take time stats on the general-search function
(defn search-stats (game q-fs depthlimit &optional (reps_allowed nil))
  (let [start 0 end 0]
    (setf start (get-internal-run-time))
    (setf results (general-search game q-fs depthlimit reps_allowed))
    (setf end (get-internal-run-time))
    (print "RUNTIME")
    (print (- end start))
    results))
