
;; countdown-to-display-starter.rkt
(require 2htdp/image)

; 
; PROBLEM:
; 
; You are asked to contribute to the design for a very simple New Year's
; Eve countdown display. You already have the data definition given below. 
; You need to design a function that consumes Countdown and produces an
; image showing the current status of the countdown. 
; 


;; Data definitions:

;; Countdown is one of:
;;  - false
;;  - Natural[1, 10]
;;  - "complete"
;; interp.
;;    false           means countdown has not yet started
;;    Natural[1, 10]  means countdown is running and how many seconds left
;;    "complete"      means countdown is over
(define CD1 false)
(define CD2 10)          ;just started running
(define CD3  1)          ;almost over
(define CD4 "complete")
#;
(define (fn-for-countdown c)
  (cond [(false? c) (...)]
        [(and (number? c) (<= 1 c) (<= c 10)) (... c)]
        [else (...)]))

;; Template rules used:
;;  - one of: 3 cases
;;  - atomic distinct: false
;;  - atomic non-distinct: Natural[1, 10]
;;  - atomic distinct: "complete"

;; Functions:

;; Countdown -> Image
;; consumes Countdown and produces an image showing the current status of the countdown
(check-expect (display-countdown false) (square 0 "solid" "white"))
(check-expect (display-countdown 5) (text (number->string 5) 24 "black"))
(check-expect (display-countdown "complete") (text "Happy New Year" 24 "red"))

#;
(define (display-countdown c) (square 0 "solid" "white")) ; stub

; <template copied from Countdown>

#;
(define (display-countdown c)
  (cond [(false? c) (...)]
        [(and (number? c) (<= 1 c) (<= c 10)) (... c)]
        [else (...)]))


(define (display-countdown c)
  (cond [(false? c)
         (square 0 "solid" "white")]
        [(and (number? c) (<= 1 c) (<= c 10))
         (text (number->string c) 24 "black")]
        [else
         (text "Happy New Year" 24 "red")]))
