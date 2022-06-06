
;; best-starter.rkt

; 
; PROBLEM:
; 
; Using the CityName data definition below design a function
; that produces true if the given city is the best in the world. 
; (You are free to decide for yourself which is the best city 
; in the world.)
; 


;; Data definitions:


;; CityName is String
;; interp. the name of a city
(define CN1 "Boston")
(define CN2 "Vancouver")

#;
(define (fn-for-city-name cn)
  (... cn))

;; Template rules used:              For the first part of the course
;;   atomic non-distinct: String     we want you to list the template
;;                                   rules used after each template.
;;

;; Functions:

;; CityName -> Boolean
;; produces true if the given city is the best in the world (arbitrary)
(check-expect (best? "Best") true)
(check-expect (best? "Other") false)

;(define (best? cn) true)  ; stub

;(define (best? cn)   ; template
;  (... cn))

(define (best? cn)
  (string=? cn "Best"))
