(require 2htdp/image)
(require 2htdp/universe)

;; traffic-light-starter.rkt

; 
; PROBLEM:
; 
; Design an animation of a traffic light. 
; 
; Your program should show a traffic light that is red, then green, 
; then yellow, then red etc. For this program, your changing world 
; state data definition should be an enumeration.
; 
; Here is what your program might look like if the initial world 
; state was the red traffic light:
; .
; Next:
; .
; Next:
; .
; Next is red, and so on.
; 
; To make your lights change at a reasonable speed, you can use the 
; rate option to on-tick. If you say, for example, (on-tick next-color 1) 
; then big-bang will wait 1 second between calls to next-color.
; 
; Remember to follow the HtDW recipe! Be sure to do a proper domain 
; analysis before starting to work on the code file.
; 
; Note: If you want to design a slightly simpler version of the program,
; you can modify it to display a single circle that changes color, rather
; than three stacked circles. 
; 



;; My world program  (make this more specific)

;; =================
;; Constants:

(define WIDTH 200)
(define HEIGHT 600)
(define MTS (empty-scene WIDTH HEIGHT "black"))
(define RATE 1)
(define SIZE  100)
(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))


;; =================
;; Data definitions:

;; TrafficLight is one of:
;; - "red"
;; - "yellow"
;; - "green"
;; imterp. the active traffic light

;; <examples redundant>

#;
(define (fn-for-traffic-light tl)
  (cond [(string=? tl "red") (...)]
        [(string=? tl "yellow") (...)]
        [(string=? tl "green") (...)]))

;; Template rules:
;; - one of:
;; - atomic distinct: "red"
;; - atomic distinct: "yellow"
;; - atomic distinct: "green"

;; =================
;; Functions:

;; TrafficLight -> TrafficLight
;; starts with red light, then switches in order
;; 
(define (main tl)
  (big-bang tl                   ; TrafficLight
            (on-tick next-light RATE)   ; TrafficLight -> TrafficLight
            (to-draw render)))   ; TrafficLight -> Image


;; TrafficLight -> TrafficLight
;; returns the next light in sequence
(check-expect (next-light "red") "green")
(check-expect (next-light "green") "yellow")
(check-expect (next-light "yellow") "red")

#;
(define (next-light tl) "green") ;stub

;; <template copied>

(define (next-light tl)
  (cond [(string=? tl "red") "green"]
        [(string=? tl "yellow") "red"]
        [(string=? tl "green") "yellow"]))


;; TrafficLight -> Image
;; draws three circles representing lights
;; the active light is the only one filled


#;
(define (render tl) (above (circle SIZE "solid" "red")
                           (circle SIZE "outline" "yellow")
                           (circle SIZE "outline" "green")))

;; <template copied>

(define (render tl)
  (cond [(string=? tl "red")
         (place-image (above (circle SIZE "solid" "red")
                             (circle SIZE "outline" "yellow")
                             (circle SIZE "outline" "green"))
                      CTR-X
                      CTR-Y
                      MTS)]
        [(string=? tl "yellow")
         (place-image (above (circle SIZE "outline" "red")
                             (circle SIZE "solid" "yellow")
                             (circle SIZE "outline" "green"))
                      CTR-X
                      CTR-Y
                      MTS)]
        [(string=? tl "green")
         (place-image (above (circle SIZE "outline" "red")
                             (circle SIZE "outline" "yellow")
                             (circle SIZE "solid" "green"))
                      CTR-X
                      CTR-Y
                      MTS)]))
