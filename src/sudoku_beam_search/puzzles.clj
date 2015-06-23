(ns sudoku-beam-search.puzzles
  (:gen-class))

(defvar *puzzle1* (make-array '(9 9)
			      :initial-contents
			      '((0 5 0 0 0 0 0 9 0)
				(0 0 6 0 0 4 8 2 0)
				(0 0 0 0 2 0 0 0 0)
				(5 0 0 0 0 0 0 0 6)
				(6 0 0 0 0 0 0 3 0)
				(0 3 0 0 5 0 0 7 0)
				(3 0 0 0 8 2 0 0 4)
				(0 8 0 5 3 0 9 0 0)
				(0 9 0 6 0 0 0 0 0))))