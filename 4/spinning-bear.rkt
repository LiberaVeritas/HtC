;; spinning-bears-starter.rkt

(require 2htdp/image)
(require 2htdp/universe)

; PROBLEM:
; 
; In this problem you will design another world program. In this program the changing 
; information will be more complex - your type definitions will involve arbitrary 
; sized data as well as the reference rule and compound data. But by doing your 
; design in two phases you will be able to manage this complexity. As a whole, this problem 
; will represent an excellent summary of the material covered so far in the course, and world 
; programs in particular.
; 
; This world is about spinning bears. The world will start with an empty screen. Clicking
; anywhere on the screen will cause a bear to appear at that spot. The bear starts out upright,
; but then rotates counterclockwise at a constant speed. Each time the mouse is clicked on the 
; screen, a new upright bear appears and starts spinning.
; 
; So each bear has its own x and y position, as well as its angle of rotation. And there are an
; arbitrary amount of bears.
; 
; To start, design a world that has only one spinning bear. Initially, the world will start
; with one bear spinning in the center at the screen. Clicking the mouse at a spot on the
; world will replace the old bear with a new bear at the new spot. You can do this part 
; with only material up through compound. 
; 
; Once this is working you should expand the program to include an arbitrary number of bears.
; 
; Here is an image of a bear for you to use: .


;; CONSTANTS

;; image
(define BEAR-IMG .)
;; rotation speed
(define SPEED 15)
;; WIDTH HEIGHT
(define WIDTH 1000)
(define HEIGHT 1000)
(define MTS (empty-scene WIDTH HEIGHT))

;; CHANGING

;; number of bears
;; angle of bears
;; bear location/mouse click


(define-struct bear (x y angle))
;; Bear is (make-bear Integer[0, WIDTH] Integer[0, HEIGHT] Integer[0, 359])
;; interp. a bear at position x y with angle angle

(define B1 (make-bear 10 20 40))
#;
(define (fn-for-bear b)
  (... (bear-x b)
       (bear-y b)
       (bear-angle b)))

;; Template rules:
;; - compound: 3


;; ListOfBear is one of:
;; - empty
;; - (cons Bear ListOfBear)
;; interp. an arbitrary number of bears

(define LB1 empty)
(define LB2 (cons (make-bear 10 20 30) empty))
(define LB3 (cons (make-bear 0 0 0) (cons (make-bear 10 20 30) empty)))
#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else (... (fn-for-bear (first lob))
                   (fn-for-lob (rest lob)))]))


;; Template rules:
;; one of: 2
;; - atomic distinct: empty
;; - compound: (cons Bear ListOfBear)
;; - reference: (first lob) is Bear
;; - self-reference: (rest lob) is ListOfBear



;; BIG BANG

;; on-tick
;; to-draw
;; on-mouse


;; ListOfBear -> ListOfBear
;; Start with an empty scene
;; clicking on scene causes a spinning bear to appear at the click location
;; additional clicks produces additional spinning bears

;; (main empty)

(define (main lob)
  (big-bang lob (on-tick spin-bears)     ; ListOfBear -> ListOfBear
                (to-draw draw-bears)     ; ListOfBear -> Image
                (on-mouse new-bear)))    ; ListOfBear Integer Integer MouseEvent -> ListOfBear

;; ListOfBear -> ListOfBear
;; rotate each bear in list by SPEED
(check-expect (spin-bears empty) empty)
(check-expect (spin-bears (cons (make-bear 0 0 0) empty))
              (cons (spin (make-bear 0 0 0)) empty))
(check-expect (spin-bears (cons (make-bear 0 0 0)
                                (cons (make-bear 10 20 30) empty)))
              (cons (spin (make-bear 0 0 0))
                    (cons (spin (make-bear 10 20 30)) empty)))
#;
(define (spin-bears lob) empty) ; stub

;; template copied

(define (spin-bears lob)
  (cond [(empty? lob) empty]
        [else (cons (spin (first lob))
                    (spin-bears (rest lob)))]))

;; Bear -> Bear
;; adds to the bears angle by SPEED modulo 359
(check-expect (spin (make-bear 0 0 0)) (make-bear 0 0 SPEED))
(check-expect (spin (make-bear 0 0 359)) (make-bear 0 0 (- SPEED 1)))

#;
(define (spin b) (make-bear 0 0 0)) ;stub

;; template copied

(define (spin b)
  (make-bear (bear-x b)
             (bear-y b)
             (modulo (+ (bear-angle b) SPEED) 360)))

;; ListOfBear -> Image
;; places a bear image for each bear at their locations and angles
(check-expect (draw-bears empty) MTS)
(check-expect (draw-bears (cons (make-bear 0 0 0) empty))
              (place-bear (make-bear 0 0 0) MTS))
(check-expect (draw-bears (cons (make-bear 0 0 0)
                                (cons (make-bear 10 20 30) empty)))
              (place-bear (make-bear 0 0 0)
                          (place-bear (make-bear 10 20 30) MTS)))

#;
(define (draw-bears lob) MTS) ;stub

;; template copied

(define (draw-bears lob)
  (cond [(empty? lob) MTS]
        [else (place-bear (first lob)
                          (draw-bears (rest lob)))]))


;; Bear Scene -> Image
;; draws a bear at its position with its angle onto a scene
(check-expect (place-bear (make-bear 0 0 0) MTS)
              (place-image BEAR-IMG 0 0 MTS))

#;
(define (place-bear b s) (place-image BEAR-IMG 0 0 MTS)) ;stub


;; template copied

(define (place-bear b scene)
    (place-image (rotate (bear-angle b) BEAR-IMG)
                 (bear-x b)
                 (bear-y b)
                 scene))


;; ListOfBear Integer Integer MouseEvent -> ListOfBear
;; when mouse is clicked on scene, add a new bear at click location to list

#;
(define (new-bear lob x y me) empty) ;stub

;; template copied

(define (new-bear lob x y me)
  (if (string=? me "button-down")
      (cond [(empty? lob) (cons (make-bear x y 0) empty)]
            [else (cons (make-bear x y 0) lob)])
      lob))


