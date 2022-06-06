
;; countdown-starter.rkt

; 
; PROBLEM:
; 
; Consider designing the system for controlling a New Year's Eve
; display. Design a data definition to represent the current state 
; of the countdown, which falls into one of three categories: 
; 
;  - not yet started
;  - from 10 to 1 seconds before midnight
;  - complete (Happy New Year!)
; 


;; Countdown is one of:
;; - false
;; - Integer[1, 10]
;; - "complete"
;; interp.
;; - false means not yet started
;; - Integer[1, 10] means started and at which second
;; - "complete" means finished

(define c1 false)
(define c2 5)
(define c3 "complete")

(define (fn-for-countdown c)
  (cond [(false? c) (...)]
        [(and (number? c) (<= 1 c) (<= c 10)) (... c)] ; can just be [(number? c) (... c)]
        [else (...)]))

;; Template rules:
;; - one-of: 3 cases
;; - atomic distinct: false
;; - atomic non-distince: Integer[1, 10]
;; - atomic distinct: "complete"
