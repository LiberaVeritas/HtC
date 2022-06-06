;; HtDD Design Quiz

;; Age is Natural
;; interp. the age of a person in years
(define A0 18)
(define A1 25)

#;
(define (fn-for-age a)
  (... a))

;; Template rules used:
;; - atomic non-distinct: Natural


; Problem 1:
; 
; Consider the above data definition for the age of a person.
; 
; Design a function called teenager? that determines whether a person
; of a particular age is a teenager (i.e., between the ages of 13 and 19).


;; Age -> true
;; returns true if age is between 13 and 19 inclusive
(check-expect (teenager? 10) false)
(check-expect (teenager? 13) true)
(check-expect (teenager? 15) true)
(check-expect (teenager? 19) true)
(check-expect (teenager? 26) false)

#;
(define (teenager? age) true) ; stub

;; <template copied from Age>

(define (teenager? age)
  (and (<= 13 age)
       (<= age 19)))



; Problem 2:
; 
; Design a data definition called MonthAge to represent a person's age
; in months.


;; MonthAge is Natural
;; interp. age of a person in months: 12 times the age
(define ma0 0)
(define ma1 12)
(define ma2 120)

#;
(define (fn-for-month-age age)
  (... age))

;; Template rules:
;; - atomic non-distinct: Natural


; Problem 3:
; 
; Design a function called months-old that takes a person's age in years 
; and yields that person's age in months.
; 


;; Age -> MonthAge
;; returns 12 times the age
(check-expect (months-old 0) 0)
(check-expect (months-old 5) 60)
(check-expect (months-old 12) 144)

#;
(define (months-old age) ; stub
  (... age))

;; <template copied from Age>

(define (months-old age)
  (* 12 age))


; Problem 4:
; 
; Consider a video game where you need to represent the health of your
; character. The only thing that matters about their health is:
; 
;   - if they are dead (which is shockingly poor health)
;   - if they are alive then they can have 0 or more extra lives
; 
; Design a data definition called Health to represent the health of your
; character.
; 
; Design a function called increase-health that allows you to increase the
; lives of a character.  The function should only increase the lives
; of the character if the character is not dead, otherwise the character
; remains dead.


;; Health is one of:
;; - false
;; - Natural
;; interp. false means character is dead
;; otherwise, the number indicates the extra lives

(define h1 false)
(define h2 0)
(define h3 10)

#;
(define (fn-for-health h)
  (if (false? h)
      (...)
      (... h)))

;; Template rules used:
;; - one of:
;; - atomic distinct: false
;; - atomic non-distinct: Natural



;; Health -> Health
;; increases the extra lives by one unless dead
(check-expect (increase-health false) false)
(check-expect (increase-health 0) 1)
(check-expect (increase-health 10) 11)

#;
(define (increase-health h) false) ; stub

;; <template copied from Healht>

(define (increase-health h)
    (if (false? h)
      false
      (+ h 1)))
