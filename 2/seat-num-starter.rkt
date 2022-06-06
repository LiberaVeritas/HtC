
;; seat-num-starter.rkt

; 
; PROBLEM:
; 
; Imagine that you are designing a program to manage ticket sales for a
; theatre. (Also imagine that the theatre is perfectly rectangular in shape!) 
; 
; Design a data definition to represent a seat number in a row, where each 
; row has 32 seats. (Just the seat number, not the row number.)
;  


;; Seat is Integer[1, 32]
;; interp. the seat number in a row
(define sn1 1)   ; first
(define sn32 32) ; last

(define (fn-for-seat sn)
  (... sn))

;; Template rune: atomic non-distinct: Integer[1, 32]
