
;; demolish-starter.rkt

;; =================
;; Data definitions:

; 
; PROBLEM A:
; 
; You are assigned to develop a system that will classify 
; buildings in downtown Vancouver based on how old they are. 
; According to city guidelines, there are three different classification levels:
; new, old, and heritage.
; 
; Design a data definition to represent these classification levels. 
; Call it BuildingStatus.
; 


;; BuildingStatus is one of:
;; - "new"
;; - "old"
;; - "heritage"
;; interp. the age status of a building

;; <examples are redundant>

#;
(define (fn-for-buildilng-status bs)
  (cond [(string=? bs "new") (...)]
        [(string=? bs "old") (...)]
        [(string=? bs "heritage") (...)]))

;; Template rules:
;; - one of:
;; - atomic distinct: "new"
;; - atomic distinct: "old"
;; - atomic distinct: "heritage"


;; =================
;; Functions:

; 
; PROBLEM B:
; 
; The city wants to demolish all buildings classified as "old". 
; You are hired to design a function called demolish? 
; that determines whether a building should be torn down or not.
; 


;; BuildingStatus -> Boolean
;; determines whether a building should be demolished or not
(check-expect (demolish? "old") true)
(check-expect (demolish? "new") false)
(check-expect (demolish? "heritage") false)

#;
(define (demolish? bs) true) ; stub

;; <template copied from BuildingStatus>

#;
(define (demolish? bs)
  (cond [(string=? bs "new") (...)]         ; template
        [(string=? bs "old") (...)]
        [(string=? bs "heritage") (...)]))


(define (demolish? bs)
  (string=? bs "old"))
