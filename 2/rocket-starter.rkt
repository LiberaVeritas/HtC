
;; rocket-starter.rkt

;; =================
;; Data definitions:

; 
; PROBLEM A:
; 
; You are designing a program to track a rocket's journey as it descends 
; 100 kilometers to Earth. You are only interested in the descent from 
; 100 kilometers to touchdown. Once the rocket has landed it is done.
; 
; Design a data definition to represent the rocket's remaining descent. 
; Call it RocketDescent.
; 


;; RocketDescent is Natural[0, 100]
;; interp. distance of rocket from earth in km
(define rd1 100)
(define rd2 50)
(define rd3 0)

#;
(define (fn-for-rocket-descent rd)
  (... rd))

;; Template rules used:
;; - atomic non-distinct: Natural[0, 100]


; 
; PROBLEM B:
; 
; Design a function that will output the rocket's remaining descent distance 
; in a short string that can be broadcast on Twitter. 
; When the descent is over, the message should be "The rocket has landed!".
; Call your function rocket-descent-to-msg.
; 


;; =================
;; Functions:

;; RocketDescent -> String
;; outputs a string of the rocket's distance to Earth
;; and "The rocket has landed!" at the end
(check-expect (rocket-descent-to-msg 100) "100")
(check-expect (rocket-descent-to-msg 50) "50")
(check-expect (rocket-descent-to-msg 0) "The rocket has landed!")

#;
(define (rocket-descent-to-msg rd)  ; stub
  (... rd))

;; <template copied from RocketDescent>

(define (rocket-descent-to-msg rd)
  (if (= rd 0)
      "The rocket has landed!"
      (number->string rd)))
