(ns sudoku-beam-search.solver
  (:gen-class))

;;==============================================================
;; Sudoku Solver
(require 'sudoku-beam-search.utility)
;; All the test cases wrapped up in classes

(def testc1 (make-instance 'game :name 'test1 :board *test1*))
(def testc2 (make-instance 'game :name 'test2 :board *test2*))
(def testc3 (make-instance 'game :name 'test3 :board *test3*))
(def testc4 (make-instance 'game :name 'test4 :board *test4*))
(def testc5 (make-instance 'game :name 'test5 :board *test5*))
(def testc6 (make-instance 'game :name 'test6 :board *test6*))
(def testc7 (make-instance 'game :name 'test7 :board *test7*))
(def testc8 (make-instance 'game :name 'test8 :board *test8*))
(def testc9 (make-instance 'game :name 'test9 :board *test9*))

;; Limits of search
(defvar expand_limit)
(setf expand_limit 200000)
(defvar max-runtime)
(setf max-runtime 30000000)


;; A quick function to change the sudoku dimensions
(defun reset-dims (x y)
  (progn 
    (setf XBLOCKS x)
    (setf YBLOCKS y)
    (setf MAXX (* XBLOCKS YBLOCKS))
    (setf MAXY (* XBLOCKS YBLOCKS))))

;; A function to generate the possible values for each cell
;; (I saw the one in sudoku-basics after I had already written this)
(defun gen-cell-values ()
  (loop for i from 1 to (* XBLOCKS YBLOCKS) collect i))

;; Lists the possible values for the cell at (aref grid row col)
(defun list-valid (grid row col)
  (let ((valid (gen-cell-values)) 
	(box_row (* (floor (/ row YBLOCKS)) YBLOCKS)) 
	(box_col (* (floor (/ col XBLOCKS)) XBLOCKS)))
    (loop for test_row from 0 to (- MAXY 1) when (not (= test_row row)) do
	  (setf valid (remove (aref grid test_row col) valid)))
    (loop for test_col from 0 to (- MAXX 1) when (not (= test_col col)) do
	  (setf valid (remove (aref grid row test_col) valid)))
    (loop for test_row from box_row to (+ box_row (- YBLOCKS 1)) do
	  (loop for test_col from box_col to (+ box_col (- XBLOCKS 1)) do
		(if (not (and (= test_row row) (= test_col col)))
		    (setf valid (remove (aref grid test_row test_col) valid)))))
    valid))

;; Genrates a list of all possible moves from a given grid
(defun find-moves (grid)
  (let ((dim (array-dimensions grid)) (temp-valid nil))
	(loop for row from 0 to (- (nth 0 dim) 1) append
	      (loop for col from 0 to (- (nth 1 dim) 1) 
		    when (EMPTY-CELL (aref grid row col)) collect
		    (progn
		      (setf temp-valid (list-valid grid row col))
		      (if (null temp-valid) (return-from find-moves nil))
		      (list (list row col) temp-valid))))))
		  

;; My thought is that the search can be designed to not explore invalid states,
;; and such I only have to check if the entire board is filled to check for goal
;; state
;; is-full is the goal test in my search
(defun is-full (grid)
  (let ((dim (array-dimensions grid)) )
  (loop for row from 0 to (- (nth 0 dim) 1) do
	(loop for col from 0 to (- (nth 1 dim) 1) do
	      (if (EMPTY-CELL (aref grid row col))
		  (return-from is-full nil)))))
  t)

;; Generates a new state based on a state and a move
(defun apply-move (grid move)
  (let ((row (car (car move))) (col (cadr (car move))) (values (cadr move)) 
	(temp nil))
    (loop for value in values collect
	  (progn
	    (setf temp (COPY-ARRAY grid))
	    (setf (aref temp row col) value)
	    (list temp (length values))))))
  
  
;; Generates all new states from one state
(defun successor-states (grid)
  (let ((moves (find-moves grid)))
    (loop for move in moves append
	    (apply-move grid move))))

;; Generates all successor games from the given game, checking for and removing
;; duplicate states
;; Correctly sets the value of sibling-games
(defun expand-game-best (game closed)
  (let ((boards (successor-states (game-board game))))
    (loop for board in boards collect
	    (if (not (gethash (prin1-to-string (car board)) closed))
		(progn 
		  (setf (gethash (prin1-to-string (car board)) closed) t)
		  (make-instance 'game 
				 :name (game-name game) 
				 :board (car board) :parent game 
				 :depth (+ (game-depth game) 1)
				 :siblings (cadr board)))))))
	      
;; Generates all successor games, ignoring duplicate states
;; correctly sets the value of sibling-games to the number
;; of other states that were generated to the same square
(defun expand-game (game)
  (let ((boards (successor-states (game-board game))))
    (loop for board in boards collect
	  (make-instance 'game 
			 :name (game-name game) 
			 :board (car board) :parent game 
			 :depth (+ (game-depth game) 1)
			 :siblings (cadr board)))))

;; The main search function the arguements are:
;; game - Start state (has to be a game class)
;; q-fn - Queueing function (q-fn old new)
;; depthlimit - Depth limit (I never used it for sudoku)
;; reps_allowed - controls if duplicate state checking is on or off
(defun general-search (game q-fn depthlimit 
			  &optional (reps_allowed nil))
  (let ((closed (make-hash-table :test #'equal))
	(goal nil) (open (list game)) (to_expand nil) (num_exp 0)
	(new_nodes nil) (num_gen 0) (start-time (get-internal-run-time)))
    (loop while (and (not goal) open) do
	  (setf to-expand (pop open))
	  (incf num_exp)
	  ;(print open)
	  ;(print to-expand)
	  (cond
	   ((null to-expand) (return-from basic-search open))
	   ((is-full (game-board to-expand)) (setf goal to-expand))
	   ((and depthlimit (> (game-depth to-expand) depthlimit)) t)
	   (reps_allowed (progn
			   (setf new_nodes (expand-game to-expand))
			   (setf num_gen (+ num_gen (length new_nodes)))
			   (setf open (funcall q-fn open new_nodes))))
	   (t (progn
		(setf new_nodes (expand-game-best to-expand closed))
		(setf num_gen (+ num_gen (length new_nodes)))
		(setf open (funcall q-fn open new_nodes)))))
	  (if (> num_exp expand_limit) (setf goal "EXCEEDED MAX NODES"))
	  (if (> (- (get-internal-run-time) start-time) max-runtime)
	      (setf goal "EXCEEDED MAX TIME")))
    (print "NUM. EXPANDED")
    (print num_exp)
    (print "NUM. GENERATED")
    (print num_gen) 
    goal))

;; BFS Queueing function, (removes nil states if they exist)
(defun q-bfs (old new)
  (remove-if-not (lambda (x) x)
		 (append old new)))
;; DFS Queueing function, (also removes any nil states)
(defun q-dfs (old new)
  (remove-if-not (lambda (x) x)
		 (append new old)))

;; Helper function for BESTFS
;; inserts a game into old-games maintaining ascending order of 
;; (sibling-games i)
(defun insert-game (game old-games)
  (cond
   ((null old-games) (list game))
   ((<= (sibling-games game) (sibling-games (car old-games))) 
    (cons game old-games))
   (t (cons (car old-games) (insert-game game (cdr old-games))))))


;; Another helper function for BESTFS
;; same as insert-game, except it doesn't fill up the stack for
;; large problems. (I also tried a tail recursive version, but the loop 
;; was faster)  
(defun insert-game-loop (new-game old-games)
  (let ((old-game nil) (remaining old-games) (numOld (length old-games))) 
    (if (null old-games) (return-from insert-game-loop (list new-game)))
    (loop for i from 0 to (- numOld 1) do
	  (setf old-game (car remaining))
	  (if (<= (sibling-games new-game) (sibling-games old-game))
	      (return-from insert-game-loop 
		(append (butlast old-games (- numOld i)) 
			(cons new-game (last old-games (- numOld i))))))
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
(defun q-bestfs (old new)
  (let ((sorted old))
    (loop for game in new do
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
(defun q-beam (old new)
  (let ((sorted nil) (templist nil))
    (loop for game in new do
	  (if game
	      (progn
		(setf templist (insert-game-loop game sorted))
		(setf sorted 
		      (subseq templist 0 
		      (min 
		       (length templist)
		       (max (sibling-games (car templist)) 1)))))))
    (append sorted old)))

;; A wrapper to take time stats on the general-search function
(defun search-stats (game q-fs depthlimit &optional (reps_allowed nil))
  (let ((start 0) (end 0))
    (setf start (get-internal-run-time))
    (setf results (general-search game q-fs depthlimit reps_allowed))
    (setf end (get-internal-run-time))
    (print "RUNTIME")
    (print (- end start))
    results))