
;; boolean-list-starter.rkt

;; =================
;; Data definitions:

; 
; PROBLEM A:
; 
; Design a data definition to represent a list of booleans. Call it ListOfBoolean. 
; 


;; ListOfBoolean is one of:
;; - empty
;; (cons Boolean ListOfBoolean)
;; interp. a list of booleans

(define L1 empty)
(define L2 (cons true empty))
(define L3 (cons false (cons true empty)))

(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else (... (first lob)
              (fn-for-lob (rest lob)))]))

;; Template rules:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Boolean ListOfBoolean)
;; - self-reference: (rest lob) is ListOfBoolean


;; =================
;; Functions:

; 
; PROBLEM B:
; 
; Design a function that consumes a list of boolean values and produces true 
; if every value in the list is true. If the list is empty, your function 
; should also produce true. Call it all-true?
; 


;; ListOfBoolean -> Boolean
;; produces true if all values in list is true. Empty also produces true.
(check-expect (all-true? empty) true)
(check-expect (all-true? (cons false empty)) false)
(check-expect (all-true? (cons true empty)) true)
(check-expect (all-true? (cons true (cons false empty))) false)

#;
(define (all-true? lob) true) ;stub
  
#;
(define (all-true? lob) ;template
  (cond [(empty? lob) (...)]
        [else (... (first lob)
              (fn-for-lob (rest lob)))]))

(define (all-true? lob)
  (cond [(empty? lob) true]
        [else (if (false? (first lob))
                  false
                  (all-true? (rest lob)))]))
