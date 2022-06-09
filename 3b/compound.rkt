
;; compound-starter.rkt

; 
; PROBLEM:
; 
; Design a data definition to represent hockey players, including both 
; their first and last names.
; 


(define-struct player (first last))
;; Player is (make-player String String)
;; interp. hockey player by name
(define p1 (make-player "James" "Jameson"))

(define (fn-for-player p)
  (... (player-first p)   ;String
       (player-last p)))  ;String

;; Template rules:
;; compound: 2 fields
