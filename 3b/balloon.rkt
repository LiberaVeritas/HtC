
;; water-balloon-starter.rkt

; PROBLEM:
; 
; In this problem, we will design an animation of throwing a water balloon.  
; When the program starts the water balloon should appear on the left side 
; of the screen, half-way up.  Since the balloon was thrown, it should 
; fly across the screen, rotating in a clockwise fashion. Pressing the 
; space key should cause the program to start over with the water balloon
; back at the left side of the screen. 
; 
; NOTE: Please include your domain analysis at the top in a comment box. 
; 
; Use the following images to assist you with your domain analysis:
; 
; 
; 1)
; 2).
; .
; 3)
; .
; 4)
; 
; .
;     
; 
; Here is an image of the water balloon:
; (define WATER-BALLOON.)
; 
; 
; 
; NOTE: The rotate function wants an angle in degrees as its first 
; argument. By that it means Number[0, 360). As time goes by your balloon 
; may end up spinning more than once, for example, you may get to a point 
; where it has spun 362 degrees, which rotate won't accept. 
; 
; The solution to that is to use the modulo function as follows:
; 
; (rotate (modulo ... 360) (text "hello" 30 "black"))
; 
; where ... should be replaced by the number of degrees to rotate.
; 
; NOTE: It is possible to design this program with simple atomic data, 
; but we would like you to use compound data.
; 
; 
; 
; 
; Constants:
; width, height, MTS, ctr-y
; balloon image
; rotation speed
; speed
; 
; 
; 
; Changing:
; balloon position x
; angle of rotation
; 
; 
; 
; 
; big bang:
; on-tick
; on-key
; to-draw


(require 2htdp/image)
(require 2htdp/universe)

;; Moves an image of a water balloon across the screen from left to right
;; The image rotates as it moves. The space key resets the position, as does reaching the end

;; =================
;; Constants:

(define WATER-BALLOON.)
(define WIDTH 600)
(define HEIGHT 400)
(define MTS (empty-scene WIDTH HEIGHT))
(define CTR-Y (/ HEIGHT 2))
(define SPEED 5)
(define RATE -5)

;; =================
;; Data definitions:

(define-struct balloon (pos ang))
;; Balloon is (make-balloon Natural[0, WIDTH] Natural[0, 360])
;; interp. the water balloon at horizontal position pos and rotation angle ang

(define B1 (make-balloon 0 0))
(define B2 (make-balloon 50 360))
(define B3 (make-balloon 100 50))

#;
(define (fn-for-balloon b)
  (.. (balloon-pos b)
      (balloon-ang b)))

;; Template rules:
;; - compound: 2 fields

;; =================
;; Functions:

;; Balloon -> Balloon
;; starts with a balloon at 0 and CTR-Y and calls functions to move and rotate it
;; 
(define (main b)
  (big-bang b                    ; Balloon
            (on-tick   tock)     ; Balloon -> Balloon
            (to-draw   render)   ; Balloon -> Image
            (on-key    reset)))  ; Balloon KeyEvent -> Balloon

;; Balloon -> Balloon
;; takes a balloon and returns the next one with position and angle changed
;; when balloon position is greater than WIDTH, resets to start
;; when angle reaches 360, resets to 0 and adds
(check-expect (tock (make-balloon 0 0)) (make-balloon (+ 0 SPEED) (modulo (+ 0 RATE) 360)))
(check-expect (tock (make-balloon WIDTH 360)) (make-balloon (+ 0 SPEED) (modulo (+ 0 RATE) 360)))
(check-expect (tock (make-balloon (- WIDTH 1) 359)) (make-balloon (+ 0 (- SPEED 1)) (modulo (+ 0 (- RATE 1)) 360)))
(check-expect (tock (make-balloon 50 50)) (make-balloon (+ 50 SPEED) (modulo (+ 50 RATE) 360)))

#;
(define (tock b) (make-balloon 3 5)) ;stub

;; <template copied>

(define (tock b)
  (make-balloon (modulo (+ (balloon-pos b) SPEED) WIDTH)
                (modulo (+ (balloon-ang b) RATE) 360)))


;; Balloon -> Image
;; takes a Balloon and places its image according to its position and angle
(check-expect (render (make-balloon 0 0)) (place-image WATER-BALLOON 0 CTR-Y MTS))
(check-expect (render (make-balloon 50 50)) (place-image (rotate 50 WATER-BALLOON) 50 CTR-Y MTS))
#;
(define (render b) WATER-BALLOON) ;stub

;; <template copied>

(define (render b)
  (place-image (rotate (balloon-ang b) WATER-BALLOON)
               (balloon-pos b)
               CTR-Y
               MTS))


;; Balloon KeyEvent -> Balloon
;; takes a balloon and resets its position and angle to the start when space pressed
(check-expect (reset (make-balloon 50 50) " ") (make-balloon 0 0))
(check-expect (reset (make-balloon 50 50) "a") (make-balloon 50 50))

#;
(define (reset b ke) (make-balloon 0 0))

;; <template copied>

(define (reset b ke)
  (if (string=? ke " ")
      (make-balloon 0 0)
      b))
