;;==============================================================
;; Utility 

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
                [3 1 2 -]])

;; A board with 7 missing numbers
(def test1 [[- 1 2 -]
            [3 2 1 -]
            [- - 3 1]
            [1 3 - -]])

;; A board with 10 missing numbers
(def test2 [[- 4 - -]
            [1 3 - -]
            [- - 1 3]
            [- - 2 -]])

;; A board with 12 missing numbers
(def test3 '((- - 1 -)
             (3 - - -)
             (- - - 1)
             (- 2 - -)))

;; Another board with 12 missing numbers
(def test4 '((- - 4 -) (3 - - -) (- - - 1) (- 2 - -)))


;; NOTE that for *test5* and *test6* you will need to change
;; the defvar/setq for XBLOCKS to 3 and YBLOCKS to 2, and reload 
;; this file to recompute MAXX, MAXY, and VALUES.

;; A 6x6 board with 8 missing numbers
(def test5 '((4 - 2 1 6 5)
             (6 5 - 4 - -)
             (- 1 5 6 4 3)
             (3 6 1 2 5 4)
             (- 2 4 - 1 6)
             (1 4 6 5 3 -)))

;; A 6x6 board with 16 missing numbers
(def test6 '((1 - - - - 2)
             (5 - 1 2 - 4)
             (3 2 - - 1 5)
             (- 5 - 1 2 6)
             (2 - - 5 - 1)
             (- 1 - - 5 3)))

;; NOTE that for *test7*, *test8*, and *test9* you will need to change
;; the defvar/setq for XBLOCKS to 3 and YBLOCKS to 3, and reload 
;; this file to recompute MAXX, MAXY, and VALUES.

;; An "easy" standard 9x9 board
(def test7 '((6 - - 7 - 3 - - 9) 
             (2 - - - - - - - 4)
             (- 3 - 9 - 1 - 2 -) 
             (- 5 - 2 - 6 - 8 -)
             (8 - - - 3 - - - 2) 
             (- 1 - 4 - 9 - 6 -)
             (- 2 - 5 - 4 - 7 -) 
             (3 - - - - - - - 6)
             (1 - - 3 - 7 - - 5)))

;; A "medium" 9x9 board
(def test8    '((1 9 - - 6 - 7 - 8) (- - - - - 7 - - 5)
                (7 - - 2 3 - - - -) (- 1 - - - - 5 - -)
                (3 - 6 - - - 4 - 9) (- - 9 - - - - 7 -)
                (- - - - 1 5 - - 3) (5 - - 9 - - - - -)
                (9 - 3 - 7 - - 5 2)))


;; A "hard" 9x9 board
(def test9     '((9 - - 4 - - 6 - -) (- - 7 - - - - - 3)
                (- - - 1 2 - - - -) (1 2 - - 4 3 - 5 -)
                (7 - - - - - - - 4) (- 4 - 7 6 - - 8 9)
                (- - - - 7 1 - - -) (6 - - - - - 9 - -)
                (- - 4 - - 8 - - 2)))


;;==============================================================
;; MACROS AND GLOBAL CONSTANTS

;; How many blocks along the x and y axes?  default is 2x2 -> 4x4 board
(def XBLOCKS "Number of large blocks along the x-axis" 3)

(def YBLOCKS "Number of large blocks along the y-axis" 3)

(def MAXX (* XBLOCKS YBLOCKS))
(def MAXY (* YBLOCKS XBLOCKS))

(def VALUES "Possible values to fill in a cell" (range 1 (+ 1 (* XBLOCKS YBLOCKS))))


(defmacro empty-cell [cell]
  """Return t if a Sudoko cell is empty (i.e., contains '-)"""
  (= cell '-))

(defmacro EMPTY [board x y]
  """Return T if an x,y position on a Sudoku board is an empty cell"""
  `(and (not (< ,x 0)) (not (< ,y 0))
    (not (>= ,x MAXX)) (not (>= ,y MAXY))
    (empty-cell (aref ,board ,x ,y))))


;;==============================================================
;; DATA STRUCTURES

;; A game (i.e., a node i the search tree) is a board (i.e., game
;; state) plus some bookkeeping information.

(def game {:name nil :board nil :parent nil :depth 0 :siblings 0})


;;==============================================================
;; USEFUL UTILITY FUNCTIONS



;; Print a game board, neatly formatted with 4-space columns. 
;; See ~< ~> formatting string and behold the power of FORMAT! 
(defn print-board [b & [str t]]
  """Print a formatted Sudoku game board"""
  (loop [x (range 0 MAXX)]
    (loop [y (range 0 MAXY 1)]
      (format str "~4<~s~>" (or (get-in b [x y]) '-)))
    (format str "~%")))

;; Method for printing an object of class GAME to an output stream
;; (default standard output) - just uses PRINT-BOARD to print the board.
;; (This is so that when you look at a game in the interpreter window,
;; it won't print a big hairy defstruct object.)
(defn print-game [ggame str]
  """Print an object of class GAME to an output stream"""
  (format str "Game ~s:~%" (:name ggame))
  (print-board (:board ggame) str))





;;==============================================================
;; BOARD PROCESSING FUNCTIONS

(defn block-groups [board]
  "Take a board (array) and return a list of lists, one with the values in each block of the board"
  (for [x (range 0 XBLOCKS)] 
     (for [y (range 0 YBLOCKS)]
        (loop [i (range 0 YBLOCKS)]
            (loop [j (range 0 XBLOCKS)]
                   ;; Each block is YBLOCKS cells wide and 
                   ;; XBLOCKS cells high.
                   ;; We're on the xth horizontal block
                   ;; and the yth vertical block, so offset
                   ;; appropriately.
                   (get-in board [(+ (* y XBLOCKS) j) (+ (* x YBLOCKS) i)])
)))))

(defn row-groups [board]
  "Take a board (array) and return a list of lists, one with the values
in each row of the board"
  (loop [y 0 results []]
    (when (< y MAXY)
        (recur (+ y 1) (conj results (board y))))))


(defn column-groups [board]
  "Take a board (array) and return a list of lists, one with the values
in each column of the board"
  (for [j (range 0 MAXY)]
     (for [i (range 0 MAXX)]
        (get-in board [i j]))))


