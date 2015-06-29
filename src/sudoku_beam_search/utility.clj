(ns sudoku-beam-search.utility
  (:gen-class))

;;==============================================================
;; TEST PROBLEMS

;; A completely solved board as a reality check
(def solved [[2 4 3 1] 
             [1 3 4 2] 
             [4 2 1 3] 
             [3 1 2 4]])

;; A board with only one missing number
(def easy-test [[2 4 3 1] 
                [1 3 4 2]
                [4 2 1 3]
                [3 1 2 '-]])

;; A board with 7 missing numbers
(def test1 [['- 1 2 '-]
            [3 '2 1 '-]
            ['- '- 3 1]
            [1 3 '- '-]])

(def test1-solved [[4 1 2 3]
                   [3 2 1 4]
                   [2 4 3 1]
                   [1 3 4 2]])

;; A board with 10 missing numbers
(def test2 [['- 4 '- '-]
            [1 3 '- '-]
            ['- '- 1 3]
            ['- '- 2 '-]])

(def test2-solved [[2 4 3 1]
                   [1 3 4 2]
                   [4 2 1 3]
                   [3 1 2 4]])

;; A board with 12 missing numbers
(def test3 [['- '- 1 '-]
            [3 '- '- '-]
            ['- '- '- 1]
            ['- 2 '- '-]])

(def test3-solved [[2 4 1 3]
                   [3 1 4 2]
                   [4 3 2 1]
                   [1 2 3 4]])

;; Another board with 12 missing numbers
(def test4 [['- '- 4 '-][3 '- '- '-]['- '- '- 1]['- 2 '- '-]])
(def test4-solved [[2 1 4 3]
                   [3 4 1 2]
                   [4 3 2 1]
                   [1 2 3 4]])


;; NOTE that for *test5* and *test6* you will need to change
;; the defvar/setq for XBLOCKS to 3 and YBLOCKS to 2, and reload 
;; this file to recompute MAXX, MAXY.

;; A 6x6 board with 8 missing numbers
(def test5 [[4 '- 2 1 6 5]
            [6 5 '- 4 '- '-]
            ['- 1 5 6 4 3]
            [3 6 1 2 5 4]
            ['- 2 4 '- 1 6]
            [1 4 6 5 3 '-]])
(def test5-solved [[4 3 2 1 6 5]
                   [6 5 3 4 2 1]
                   [2 1 5 6 4 3]
                   [3 6 1 2 5 4]
                   [5 2 4 3 1 6]
                   [1 4 6 5 3 2]])

;; A 6x6 board with 16 missing numbers
(def test6 [[1 '- '- '- '- 2]
            [5 '- 1 2 '- 4]
            [3 2 '- '- 1 5]
            ['- 5 '- 1 2 6]
            [2 '- '- 5 '- 1]
            ['- 1 '- '- 5 3]])

(def test6-solved [[1 4 5 3 6 2] 
                   [5 6 1 2 3 4]
                   [3 2 4 6 1 5]
                   [4 5 3 1 2 6]
                   [2 3 6 5 4 1]
                   [6 1 2 4 5 3]])

;; NOTE that for *test7*, *test8*, and *test9* you will need to change
;; the defvar/setq for XBLOCKS to 3 and YBLOCKS to 3, and reload 
;; this file to recompute MAXX, MAXY, and VALUES.

;; An "easy" standard 9x9 board
(def test7 [[6 '- '- 7 '- 3 '- '- 9]
            [2 '- '- '- '- '- '- '- 4]
            ['- 3 '- 9 '- 1 '- 2 '-] 
            ['- 5 '- 2 '- 6 '- 8 '-]
            [8 '- '- '- 3 '- '- '- 2] 
            ['- 1 '- 4 '- 9 '- 6 '-]
            ['- 2 '- 5 '- 4 '- 7 '-]
            [3 '- '- '- '- '- '- '- 6]
            [1 '- '- 3 '- 7 '- '- 5]])

;; A "medium" 9x9 board
(def test8 [[1 9 '- '- 6 '- 7 '- 8] ['- '- '- '- '- 7 '- '- 5]
            [7 '- '- 2 3 '- '- '- '-] ['- 1 '- '- '- '- 5 '- '-]
            [3 '- 6 '- '- '- 4 '- 9] ['- '- 9 '- '- '- '- 7 '-]
            ['- '- '- '- 1 5 '- '- 3] [5 '- '- 9 '- '- '- '- '-]
            [9 '- 3 '- 7 '- '- 5 2]])


;; A "hard" 9x9 board
(def test9 [[ 9 '- '-  4 '- '-  6 '- '-] 
            ['- '-  7 '- '- '- '- '-  3]
            ['- '- '-  1  2 '- '- '- '-]
            [ 1  2 '- '-  4  3 '-  5 '-]
            [ 7 '- '- '- '- '- '- '-  4] 
            ['-  4 '-  7  6 '- '-  8  9]
            ['- '- '- '-  7  1 '- '- '-] 
            [ 6 '- '- '- '- '-  9 '- '-]
            ['- '-  4 '- '-  8 '- '-  2]])

(def test9-solved [[9 5 8 4 3 7 6 2 1] 
                   [2 1 7 9 8 6 5 4 3]
                   [4 6 3 1 2 5 8 9 7]
                   [1 2 9 8 4 3 7 5 6]
                   [7 8 6 5 1 9 2 3 4]
                   [3 4 5 7 6 2 1 8 9]
                   [8 9 2 3 7 1 4 6 5]
                   [6 3 1 2 5 4 9 7 8]
                   [5 7 4 6 9 8 3 1 2]])




;;==============================================================
;; BASIC UTILITY FUNCTIONS

(defn numrows [b]
  (count (first b)))

(defn numcols [b]
  (count b))

(defn empty-cell [cell]
  """Return true if a Sudoko cell is empty (i.e., is '-)"""
  (= cell '-))

(defn empty-loc [board row col]
  """Return T if an row,col position on a Sudoku board is an empty cell"""
  (and (not (< row 0)) (not (< col 0))
    (not (>= row (numrows board))) (not (>= col (numcols board)))
    (empty-cell (get-in board [row col]))))


;;==============================================================
;; DATA STRUCTURES

;; A game (i.e., a node i the search tree) is a board (i.e., game
;; state) plus some bookkeeping information.

(def base-game {:name nil :board nil :parent nil :depth 0 :siblings 0})


;;==============================================================
;; PRINTING FUNCTIONS

;; Print a game board, neatly formatted with 1-space columns. 
(defn print-board [b]
  """Print a formatted Sudoku game board"""
  (doseq [y (range 0 (count b))] (do
    (doseq [x (range 0 (count (first b)))]
      (printf " %s" (str (or (get-in b [y x]) "-"))))
    (printf "%n"))))

;; Method for printing a game object to stdout
(defn print-game [ggame]
  """Print an object of class GAME to an output stream"""
  (printf "Game %s:%n" (:name ggame))
  (print-board (:board ggame)))





