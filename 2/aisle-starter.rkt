
;; aisle-starter.rkt

; 
; PROBLEM:
; 
; Using the SeatNum data definition below design a function
; that produces true if the given seat number is on the aisle. 
; 


;; Data definitions:

;; SeatNum is Natural[1, 32]
;; Interp. Seat numbers in a row, 1 and 32 are aisle seats
(define SN1  1) ;aisle
(define SN2 12) ;middle
(define SN3 32) ;aisle
#;
(define (fn-for-seat-num sn)
  (... sn)) 

;; Template rules used:
;;  atomic non-distinct: Natural[1, 32]


;; Functions:

;; SeatNum -> Boolean
;; takes a SeatNum and says true if it's an aisle seat
(check-expect (aisle? 1) true)
(check-expect (aisle? 32) true)
(check-expect (aisle? 5) false)

;(define (aisle? sn) false)  ; stub

;(define (aisle? sn)  ; template
;  (... sn))


(define (aisle? sn)
  (or (= sn 1) (= sn 32)))
