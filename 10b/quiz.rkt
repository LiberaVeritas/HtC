(require 2htdp/image)

;  PROBLEM 1:
;  
;  In the lecture videos we designed a function to make a Sierpinski triangle fractal. 
;  
;  Here is another geometric fractal that is made of circles rather than triangles:
;  
;  .
;  
;  Design a function to create this circle fractal of size n and colour c.
;  


(define CUT-OFF 5)

(define blk "black")
(define red "red")

(define B CUT-OFF)
(define 2B (* B 2))

(define (circ n c)
  (circle n "outline" c))

;; Natural String -> Image
;; produce a circle fractal of size n and colour c

(check-expect (circle-fractal B blk) (circ B blk))
(check-expect (circle-fractal 2B red) (overlay (circ 2B red)
                                               (beside (circ B red)
                                                       (circ B red))))

(define (circle-fractal n c)
  (if  (<= n B)
       (circ n c)
       (local [(define sub
                 (circle-fractal (/ n 2) c))]
         (overlay (circ n c)
                  (beside sub sub)))))





;  PROBLEM 2:
;  
;  Below you will find some data definitions for a tic-tac-toe solver. 
;  
;  In this problem we want you to design a function that produces all 
;  possible filled boards that are reachable from the current board. 
;  
;  In actual tic-tac-toe, O and X alternate playing. For this problem
;  you can disregard that. You can also assume that the players keep 
;  placing Xs and Os after someone has won. This means that boards that 
;  are completely filled with X, for example, are valid.
;  
;  Note: As we are looking for all possible boards, rather than a winning 
;  board, your function will look slightly different than the solve function 
;  you saw for Sudoku in the videos, or the one for tic-tac-toe in the 
;  lecture questions. 
;  


;; Value is one of:
;; - false
;; - "X"
;; - "O"
;; interp. a square is either empty (represented by false) or has and "X" or an "O"

(define (fn-for-value v)
  (cond [(false? v) (...)]
        [(string=? v "X") (...)]
        [(string=? v "O") (...)]))

;; Board is (listof Value)
;; a board is a list of 9 Values
(define B0 (list false false false
                 false false false
                 false false false))

(define B1 (list false "X"   "O"   ; a partly finished board
                 "O"   "X"   "O"
                 false false "X")) 

(define B2 (list "X"  "X"  "O"     ; a board where X will win
                 "O"  "X"  "O"
                 "X" false "X"))

(define B3 (list "X" "O" "X"       ; a board where Y will win
                 "O" "O" false
                 "X" "X" false))

(define (fn-for-board b)
  (cond [(empty? b) (...)]
        [else 
         (... (fn-for-value (first b))
              (fn-for-board (rest b)))]))


(define-struct tree (c x o))
;; Tree is one of:
;; - empty
;; - (make-tree Board Tree Tree)
;; interp. a tree of all possible boards from the current one
;; x is always board with next spot filled with x, and same for o

(define (fn-for-tree t)
  (cond [(empty? t) (...)]
        [else
         (... (fn-for-board (tree-c t))
              (fn-for-tree (tree-x t))
              (fn-for-tree (tree-o t)))]))

(define T1 (make-tree B1 empty empty))
(define T2 (make-tree B2 empty empty))
(define T3 (make-tree B3 empty empty))
(define T4 (make-tree B1 T2 T3))


;; (Board X X -> X) X Tree -> X
;; fold for tree

(check-expect (fold-tree make-tree empty T4) T4)

(define (fold-tree fn base t)
  (cond [(empty? t) base]
        [else
         (fn (tree-c t)
             (fold-tree fn base (tree-x t))
             (fold-tree fn base (tree-o t)))]))



;; Tree -> List
;; list all board in the tree

(check-expect (list-tree T4) (list B1 B2 B3))

(define (list-tree t)
  (local [(define (helper b x o)
            (cons b (append x o)))]
    (fold-tree helper empty t)))



;; Board -> Boolean
;; true if board is full

(check-expect (filled? (fill-next B2 "X")) true)

(define (filled? b)
  (not (member false b)))


;; Board Value -> Board
;; board with next empty spot filled with given value
;; assume board isn't filled

(check-expect (fill-next B1 "X") (cons "X" (rest B1)))

(define (fill-next b v)
  (if (false? (first b))
      (cons v (rest b))
      (cons (first b)
            (fill-next (rest b) v))))



;; Board -> Tree
;; all game states stemming from given board

(check-expect (generate-states B2) (make-tree B2
                                              (make-tree
                                               (fill-next B2 "X") empty empty)
                                              (make-tree
                                               (fill-next B2 "O") empty empty)))



(define (generate-states b)
  (cond [(filled? b) (make-tree b empty empty)]
        [else
         (make-tree b
                    (generate-states (fill-next b "X"))
                    (generate-states (fill-next b "O")))]))



;; Board -> (listof Board)
;; list of all game states from the given board

(check-expect (list-states B2) (list B2 (fill-next B2 "X") (fill-next B2 "O")))

(define (list-states b)
  (list-tree (generate-states b)))



;; Board -> (listof Board)
;; list of all filled states from given board

(define (list-filled-states b)
  (filter filled? (list-states b)))



;  PROBLEM 3:
;  
;  Now adapt your solution to filter out the boards that are impossible if 
;  X and O are alternating turns. You can continue to assume that they keep 
;  filling the board after someone has won though. 
;  
;  You can assume X plays first, so all valid boards will have 5 Xs and 4 Os.
;  
;  NOTE: make sure you keep a copy of your solution from problem 2 to answer 
;  the questions on edX.
;  



;; Value -> Boolean
;; whether given value is "X"

(define (X? v)
  (string=? "X" v))

;; Value -> Boolean
;; whether given value is "O"

(define (O? v)
  (string=? "O" v))


;; Board -> Boolean
;; whether the given board is valid (5 Xs and 4 Os)
;; assume filled board

(define (possible? b)
  (and (= 5 (length (filter X? b)))
       (= 4 (length (filter O? b)))))


;; Board -> (listof Board)
;; list of all possible filled states from given board

(define (list-possible-filled-states b)
  (filter possible? (list-filled-states b)))

