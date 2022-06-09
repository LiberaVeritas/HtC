
;; cat-starter.rkt

; 
; PROBLEM:
; 
; Use the How to Design Worlds recipe to design an interactive
; program in which a cat starts at the left edge of the display 
; and then walks across the screen to the right. When the cat
; reaches the right edge it should just keep going right off 
; the screen.
; 
; Once your design is complete revise it to add a new feature,
; which is that pressing the space key should cause the cat to
; go back to the left edge of the screen. When you do this, go
; all the way back to your domain analysis and incorporate the
; new feature.
; 
; To help you get started, here is a picture of a cat, which we
; have taken from the 2nd edition of the How to Design Programs 
; book on which this course is based.
; 
; .
; 


(require 2htdp/universe)
(require 2htdp/image)

;; A cat that walks across the screen.

;; Constants:

(define WIDTH  200)
(define HEIGHT 200)
(define CTR-Y (/ HEIGHT 2))

(define CAT-IMG .) ; a not very attractive cat

(define MTS (empty-scene WIDTH HEIGHT))


;; Data definitions:

;; Cat is Number
;; interp. x coordinate of cat (in screen coordinates)
(define C1 1)
(define C2 30)

#;
(define (fn-for-cat c)
  (... c))


;; Functions:

;; Cat -> Cat
;; start the world with initial state c, for example: (main 0)
(define (main c)
  (big-bang c                         ; Cat
            (on-tick   tock)          ; Cat -> Cat
            (to-draw   render)))      ; Cat -> Image

;; Cat -> Cat
;; Move cat to the right by 1 pixel
(check-expect (tock 0) 1)

#;
(define (tock c) 1)  ;stub

;; <template copied>

(define (tock c)
  (add1 c))

;; Cat -> Image
;; produce image with CAT-IMG placed on MTS at proper x, y position
(check-expect (render 50) (place-image CAT-IMG 50 CTR-Y MTS))
#;
(define (render c) MTS) ;stub

;; <template copied>

(define (render c)
  (place-image CAT-IMG c CTR-Y MTS))
