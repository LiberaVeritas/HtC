
;; pattern-match-starter.rkt

; Problem:
; 
; It is often useful to be able to tell whether the first part of a sequence of 
; characters matches a given pattern. In this problem you will design a (somewhat 
; limited) way of doing this.
; 
; Assume the following type comments and examples:


;; =================
;; Data Definitions:

;; 1String is String
;; interp. these are strings only 1 character long
(define 1SA "x")
(define 1SB "2")

;; Pattern is one of:
;;  - empty
;;  - (cons "A" Pattern)
;;  - (cons "N" Pattern)
;; interp.
;;   A pattern describing certain ListOf1String. 
;;  "A" means the corresponding letter must be alphabetic.
;;  "N" means it must be numeric.  For example:
;;      (list "A" "N" "A" "N" "A" "N")
;;   describes Canadian postal codes like:
;;      (list "V" "6" "T" "1" "Z" "4")
(define PATTERN1 (list "A" "N" "A" "N" "A" "N"))

;; ListOf1String is one of:
;;  - empty
;;  - (cons 1String ListOf1String)
;; interp. a list of strings each 1 long
(define LOS1 (list "V" "6" "T" "1" "Z" "4"))

; 
; Now design a function that consumes Pattern and ListOf1String and produces true 
; if the pattern matches the ListOf1String. For example,
; 
; (pattern-match? (list "A" "N" "A" "N" "A" "N")
;                 (list "V" "6" "T" "1" "Z" "4"))
; 
; should produce true. If the ListOf1String is longer than the pattern, but the 
; first part matches the whole pattern produce true. If the ListOf1String is 
; shorter than the Pattern you should produce false.       
; 
; Treat this as the design of a function operating on 2 complex data. After your 
; signature and purpose, you should write out a cross product of type comments 
; table. You should reduce the number of cases in your cond to 4 using the table, 
; and you should also simplify the cond questions using the table.
; 
; You should use the following helper functions in your solution:
; 


;; ==========
;; Functions:

;; 1String -> Boolean
;; produce true if 1s is alphabetic/numeric
(check-expect (alphabetic? " ") false)
(check-expect (alphabetic? "1") false)
(check-expect (alphabetic? "a") true)
(check-expect (numeric? " ") false)
(check-expect (numeric? "1") true)
(check-expect (numeric? "a") false)

(define (alphabetic? 1s) (char-alphabetic? (string-ref 1s 0)))
(define (numeric?    1s) (char-numeric?    (string-ref 1s 0)))





;; CROSS PRODUCT OF TYPE COMMENTS TABLE
;;
;;                                             lstb
;;                                 empty            (cons 1String LOS)                
;;                                               |
;; l   empty                       true          |    true
;; s                             --------------------------------
;; t   (cons "A"/"N" Pattern)      false         |  see function
;; a                                             |         




;; Pattern ListOf1String -> Boolean
;; consumes Pattern and ListOf1String and produces true
;; if the pattern matches the ListOf1String up to the patterns length

(check-expect (match? empty LOS1) true)
(check-expect (match? PATTERN1 empty) false)
(check-expect (match? empty empty) true)
(check-expect (match? PATTERN1 LOS1) true)
(check-expect (match? (append PATTERN1 (list "N")) LOS1) false)
(check-expect (match? PATTERN1 (append LOS1 (list "E"))) true)


(define (match? p los)
  (cond [(empty? p) true]
        [(empty? los) false]
        [(string=? (first p) "A")
         (and (alphabetic? (first los))
              (match? (rest p) (rest los)))]
        [(string=? (first p) "N")
         (and (numeric? (first los))
              (match? (rest p) (rest los)))]))
