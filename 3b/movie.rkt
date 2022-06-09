
;; movie-starter.rkt

;; =================
;; Data definitions:

(define-struct movie (name budget released))
;; Movie is (make-movie (String Integer Natural))
;; interp. is a movie's name, budget, an release year

(define (fn-for-movie m)
  (... (movie-name m)        ;String
       (movie-budget m)      ;Integer
       (movie-released m)))  ;Natural

;; Template rules:
;; compound: 3 fields

; 
; PROBLEM A:
; 
; Design a data definition to represent a movie, including  
; title, budget, and year released.
; 
; To help you to create some examples, find some interesting movie facts below: 
; "Titanic" - budget: 200000000 released: 1997
; "Avatar" - budget: 237000000 released: 2009
; "The Avengers" - budget: 220000000 released: 2012
; 
; However, feel free to resarch more on your own!
; 


;; =================
;; Functions:

;; Movie Movie -> String
;; returns movie name with the higher released field
;; and first if equal
(check-expect (most-recent (make-movie "A" 10000 2000)
                           (make-movie "B" 20000 1999))
                           "A")
(check-expect (most-recent (make-movie "A" 10000 1999)
                           (make-movie "B" 20000 2000))
                           "B")
(check-expect (most-recent (make-movie "A" 10000 2000)
                           (make-movie "B" 20000 2000))
                           "A")
#;
(define (most-recent m1 m2) "A") ;stub

;; <template copied>

(define (most-recent m1 m2)
  (if (>= (movie-released m1) (movie-released m2))
      (movie-name m1)
      (movie-name m2)))

; 
; PROBLEM B:
; 
; You have a list of movies you want to watch, but you like to watch your 
; rentals in chronological order. Design a function that consumes two movies 
; and produces the title of the most recently released movie.
; 
; Note that the rule for templating a function that consumes two compound data 
; parameters is for the template to include all the selectors for both 
; parameters.
; 

