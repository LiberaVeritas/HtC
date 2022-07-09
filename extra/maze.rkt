
; 
; In this problem set you will design a program to check whether a given simple maze is
; solvable.  Note that you are operating on VERY SIMPLE mazes, specifically:
; 
;    - all of your mazes will be square
;    - the maze always starts in the upper left corner and ends in the lower right corner
;    - at each move, you can only move down or right
; 
; Design a representation for mazes, and then design a function that consumes a maze and
; produces true if the maze is solvable, false otherwise.
; 
; Solvable means that it is possible to start at the upper left, and make it all the way to
; the lower right.  Your final path can only move down or right one square at a time. BUT, it
; is permissible to backtrack if you reach a dead end.
; 
; For example, the first three mazes below are solvable.  Note that the fourth is not solvable
; because it would require moving left. In this solver you only need to support moving down
; and right! Moving in all four directions introduces complications we are not yet ready for.
; 
;     .    .    .    .
; 
; 
; Your function will of course have a number of helpers. Use everything you have learned so far
; this term to design this program. 
; 
; One big hint. Remember that we avoid using an image based representation of information unless
; we have to. So the above are RENDERINGs of mazes. You should design a data definition that
; represents such mazes, but don't use images as your representation.
; 
; For extra fun, once you are done, design a function that consumes a maze and produces a
; rendering of it, similar to the above images.
; 





;; Solve simple square mazes

;; Constants:

(define N 5)

(define F "Finish")
(define B "Blocked")
(define O "Open")
(define P "Path")
(define S "Start")



;; Data definitions:

;; Cell is one of:
;; - "Start"
;; - "Finish"
;; - "Blocked"
;; - "Open"
;; - "Path"
;; interp. a cell in the maze


(define (fn-for-cell c)
  (cond [(string=? S c) (...)]
        [(string=? F c) (...)]
        [(string=? B c) (...)]
        [(string=? O c) (...)]
        [(string=? P c) (...)]))


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
                          (cons (make-pos e (first lst))
                                (helper e (rest lst)))]))]
           (append (helper (first lst1) lst2)
                   (cross (rest lst1) lst2)))]))

(define rows (build-list N identity))
(define cols rows)

(define positions (cross rows cols))


(define C1 (list S B B B B
                 O O B O O
                 B O B B B
                 O O B B B
                 O O O O F))

(define C2 (list S O O O O
                 O B B B O
                 O B B B O
                 O B B B O
                 O B B B F))

(define C3 (list S O O O O
                 O B B B B
                 O B B B B
                 O B B B B
                 O O O O F))

(define C4 (list S O O O O
                 O B B B O
                 O B O O O
                 O B O B B
                 B B O O F))


(define-struct mcell (pos cell))
;; Mcell is (make-mcell Pos Cell) or false

(define (fn-for-mcell mc)
  (... (fn-for-pos (mcell-pos mc))
       (mcell-cell mc)))



;; Maze is (listof Mcell)


;; (listof Pos) (listof Cell) -> Maze

(define (maze posns cells)
  (cond [(empty? posns) empty]
        [else
         
         (cons (make-mcell (first posns) (first cells))
               (maze (rest posns) (rest cells)))]))


(define M1 (maze positions C1))
(define M2 (maze positions C2))
(define M3 (maze positions C3))
(define M4 (maze positions C4))


;; Functions:


;; Pos -> Pos
;; Pos one to right, col < N-1

(define (pos-to-right pos)
  (make-pos (pos-r pos)
            (add1 (pos-c pos))))

;; Pos -> Pos
;; Pos one down, row < N-1

(define (pos-to-down pos)
  (make-pos (add1 (pos-r pos))
            (pos-c pos)))

;; Pos Maze -> Mcell
;; Mcell in Maze given Pos

(define (find-mcell pos maze)
  (local [(define r (pos-r pos))
          (define c (pos-c pos))
          (define x (+ c (* N r)))]
    (list-ref maze x)))


;; Mcell Maze -> Mcell
;; Mcell to the right of given in maze

(define (to-right mc maze)
  (cond [(= (sub1 N) (pos-c (mcell-pos mc))) false]
        [else
         (find-mcell (pos-to-right (mcell-pos mc))
                     maze)]))


;; Mcell Maze -> Mcell
;; Mcell one down from given in maze

(define (to-down mc maze)
  (cond [(= (sub1 N) (pos-r (mcell-pos mc))) false]
        [else
         (find-mcell (pos-to-down (mcell-pos mc))
                     maze)]))


;; Mcell -> Boolean
;; if mcell's cell is F

(define (finish? mc)
  (if (false? mc)
      false
      (string=? F (mcell-cell mc))))


;; Mcell -> Boolean
;; if mcell's cell is B

(define (blocked? mc)
  (if (false? mc)
      false
      (string=? B (mcell-cell mc))))


;; Maze -> Boolean
;; if maze is solvable or not

(define (solve? maze)
  (local [(define (helper maze current)
            (if (false? current)
                false
                (local [(define right (to-right current maze))
                        (define down (to-down current maze))]
                  
                  (cond [(or (finish? down) (finish? right)) true]
                        [(blocked? right) (helper maze down)]
                        [(blocked? down) (helper maze right)]
                        
                        [else
                         (or (helper maze right)
                             (helper maze down))]))))]
    (helper maze (first maze))))



;; Maze Mcell -> Maze
;; maze with given mcell filled with P cell

(define (fill maze mc)
  (cond [(empty? maze) empty]
        [(string=? (mcell-cell mc) S) maze]
        [else
         (if (equal? (first maze) mc)
             (cons (make-mcell (mcell-pos mc)
                               P)
                   (rest maze))
             (cons (first maze)
                   (fill (rest maze) mc)))]))


;; Maze -> (listof (listof Mcell))
;; list of rows of maze
#;
(define (maze-rows maze)
  (local [(define (helper mcell col)
            
          
            (cond [(empty? maze) empty]
                  [else
                   (local [(define (make-row mcell col)
                             (cond [(= (sub1 N) col) empty]
                                   [else
                                    (cons (first maze)
                                          (row (rest maze) (add1 col)))]))]
                     (cons (row maze col)))]))]))
                          

(require 2htdp/image)

;; Mcell -> Image
;; draw mcell

(define SIZE 50)

(define MTS (empty-scene (* SIZE N) (* SIZE N)))

(define (cell-color c)
  (cond [(string=? S c) "blue"]
        [(string=? F c) "green"]
        [(string=? O c) "gray"]
        ((string=? B c) "black")
        ((string=? P c) "red")))

(define (draw-mcell mc)
  (square SIZE "solid" (cell-color (mcell-cell mc))))


           
;; Maze -> Image or false
;; draw maze

(require racket/list)


(define (draw-row lomc)
  (cond ((empty? lomc) empty-image)
        (else
         (beside (draw-mcell (first lomc))
                 (draw-row (rest lomc))))))

(define (draw maze)
  (cond ((false? maze) false)
        ((empty? maze) empty-image)
        (else
         (above (draw-row (take maze N))
                (draw (drop maze N))))))

  
  ;; Maze -> Maze or false
  ;; maze with solution path

  (define (solve maze)
    (local [(define (helper maze current)
              (if (false? current)
                  false
                  (local [(define right (to-right current maze))
                          (define down (to-down current maze))
                          (define filled (fill maze current))]

                    (cond [(or (finish? right) (finish? down)) filled]
                          [(blocked? right) (helper filled down)]
                          [(blocked? down) (helper filled right)]
                          [else
                           (if (false? (helper filled right))
                               (helper filled down)
                               (helper filled right))]))))]
    
      (draw (helper maze (first maze)))))
  
  
