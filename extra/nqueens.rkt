

;; nqueens-starter.rkt


; This project involves the design of a program to solve the n queens puzzle.
; 
; This starter file explains the problem and provides a few hints you can use
; to help with the solution.
; 
; The key to solving this problem is to follow the recipes! It is a challenging
; problem, but if you understand how the recipes lead to the design of a Sudoku
; solve then you can follow the recipes to get to the design for this program.
;   
; 
; The n queens problem consists of finding a way to place n chess queens
; on a n by n chess board while making sure that none of the queens attack each
; other. 
; 
; The BOARD consists of n^2 individual SQUARES arranged in 4 rows of 4 columns.
; The colour of the squares does not matter. Each square can either be empty
; or can contain a queen.
; 
; A POSITION on the board refers to a specific square.
; 
; A queen ATTACKS every square in its row, its column, and both of its diagonals.
; 
; A board is VALID if none of the queens placed on it attack each other.
; 
; A valid board is SOLVED if it contains n queens.
; 
; 
; There are many strategies for solving nqueens, but you should use the following:
;   
;   - Use a backtracking search over a generated arb-arity tree that
;     is trying to add 1 queen at a time to the board. If you find a
;     valid board with 4 queens produce that result.
; 
;   - You should design a function that consumes a natural - N - and
;     tries to find a solution.
;     
;     
;     
; NOTE 1: You can tell whether two queens are on the same diagonal by comparing
; the slope of the line between them. If one queen is at row and column (r1, c1)
; and another queen is at row and column (r2, c2) then the slope of the line
; between them is: (/ (- r2 r1) (- c2 c1)).  If that slope is 1 or -1 then the
; queens are on the same diagonal.





; Info                                                                                     
; 
; Board
; board positions
; square - valid, queen, attacked
; N
; Attack patterns - row, col, diag
; solved/unsolved
; game tree
; 



;; Data definitions:


;; POS


(define-struct pos (r c))
;; Pos is (make-pos Natural[0, N-1] Natural[0, N-1])
;; interp. row and column of a maze

(define (fn-for-pos p)
  (... (pos-r p)
       (pos-c p)))



;; List List -> (listof Pair)
;; cartesian product of two lists of same length

(define (cross lst1 lst2)
  (cond [(empty? lst1) empty]
        [else
         (local [(define (helper e lst)
                   (cond [(empty? lst) empty]
                         [else
                          (cons (list e (first lst))
                                (helper e (rest lst)))]))]
           (append (helper (first lst1) lst2)
                   (cross (rest lst1) lst2)))]))




;; SQUARE

(define Q "Queen")
(define A "Attacked")
(define O "Open")
(define I "Invalid")

;; State is one of:
;; - Q
;; - A
;; - O
;; - I
;; interp. queen on square, attacked by another queen, or open for queen, invalid for not solution




(define-struct sq (r c state))
;; Square is  (make-square Natural[0, N-1] Natural[0, N-1] Boolean)
;; interp. square on chess board with row, col, and state



;; Square -> Boolean
;; if given square is open

(define (open? sq)
  (string=? O (sq-state sq)))

(define (valid? sq)
  (and (open? sq)
       (not (string=? I (sq-state sq)))))

(define (queen? sq)
  (string=? Q (sq-state sq)))


;; BOARD

;; Board is (list of Square)

(check-expect (new-board 5) (list
                             (make-sq 0 0 O)
                             (make-sq 0 1 O)
                             (make-sq 0 2 O)
                             (make-sq 0 3 O)
                             (make-sq 0 4 O)
                             (make-sq 1 0 O)
                             (make-sq 1 1 O)
                             (make-sq 1 2 O)
                             (make-sq 1 3 O)
                             (make-sq 1 4 O)
                             (make-sq 2 0 O)
                             (make-sq 2 1 O)
                             (make-sq 2 2 O)
                             (make-sq 2 3 O)
                             (make-sq 2 4 O)
                             (make-sq 3 0 O)
                             (make-sq 3 1 O)
                             (make-sq 3 2 O)
                             (make-sq 3 3 O)
                             (make-sq 3 4 O)
                             (make-sq 4 0 O)
                             (make-sq 4 1 O)
                             (make-sq 4 2 O)
                             (make-sq 4 3 O)
                             (make-sq 4 4 O)))


(define (new-sq rc)
  (make-sq (first rc)
           (second rc)
           O))


(define (new-board n)
  (map new-sq (cross (build-list n identity) (build-list n identity))))



;; Board Row Col -> Board
;; Board with a queen placed on board at given row and col
;; assume square is open and in board


(define (set bd r c s)
  (cond ((empty? bd) empty)
        (else
         (if (and (= r (sq-r (first bd))) (= c (sq-c (first bd))))
             (cons (make-sq r c s)
                   (rest bd))

             (cons (first bd)
                   (set (rest bd) r c s))))))



(define (bd-size bd)
  (sqrt (length bd)))



;; Attacks


;; Square Row Col -> Boolean
;; if square is attacked by queen at given row and col

(define (attacked-by? sq r c)
  (or (= r (sq-r sq))
      (= c (sq-c sq))
      (= 1 (abs (/ (- r (sq-r sq))
                   (- c (sq-c sq)))))))


;; Square -> Square
;; attacked square

(define (attack sq)
  (make-sq (sq-r sq) (sq-c sq) A))


;; Row Col -> (Square -> Square)
;; square set to attacked if attacked by queen in given row and col

(define (m-attack-from r c)
  (λ (sq)
    (if (and (open? sq)
             (attacked-by? sq r c))
        (attack sq)
        sq)))


;; Board Row Col -> Board
;; Board with square attacked by queen at row and col

(define (attack-from bd r c)
  (map (m-attack-from r c) bd))



(define B1 (new-board 8))
(define B2 (set B1 3 2 Q))
(define B3 (attack-from B2 3 2))


;; Solve



;; Board Row -> (list of Square)
;; valid sqs at given row

(define (valid-pos-row bd r)
  (filter valid? (drop bd (* (bd-size bd) r))))


;; Board -> (Square -> Board)
;; queen and attack

(define (queen-and-attack bd)
  (λ (sq)
    (attack-from (set bd (sq-r sq) (sq-c sq) Q)
                 (sq-r sq)
                 (sq-c sq))))


;; Natural ->  Board or false
;; solve or false


(define (solve n)
  (local ((define (fn1 bd queens)
            (if (= n queens)
                bd
                (fn2 (map (queen-and-attack bd)
                          (valid-pos-row bd queens))
                     (add1 queens))))

          (define (fn2 bds queens)
            (cond ((empty? bds) false)
                  (else
                   (local ((define try (fn1 (first bds) queens)))
                     (if (false? try)
                         (fn2 (rest bds) queens)
                         try))))))
          
    (fn1 (new-board n) 0)))




;; Draw



(require 2htdp/image)

;; Square -> Image
;; draw square

(define SIZE 30)


(define (sq-color sq)
  (cond [(string=? Q (sq-state sq)) "lightorange"]
        ((string=? O (sq-state sq)) "lightgray")
        ((string=? A (sq-state sq)) "lightred")))

(define (draw-sq sq)
  (overlay (square SIZE "outline" "black")
           (square SIZE "solid" (sq-color sq))))




           
;; Board -> Image or false
;; draw board

(require racket/list)


(define (draw-row los)
  (cond ((empty? los) empty-image)
        (else
         (beside (draw-sq (first los))
                 (draw-row (rest los))))))

(define (draw bd)
  (if (false? bd)
      false
      (local ((define s (bd-size bd))
          
              (define (fn bd)
                (cond ((empty? bd) empty-image)
                      (else
                       (above (draw-row (take bd s))
                              (fn (drop bd s)))))))
        (fn bd))))

  


  
