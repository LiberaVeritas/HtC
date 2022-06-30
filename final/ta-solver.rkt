;; ta-solver-starter.rkt



;  PROBLEM 1:
;  
;  Consider a social network similar to Twitter called Chirper. Each user has a name, a note about
;  whether or not they are a verified user, and follows some number of people. 
;  
;  Design a data definition for Chirper, including a template that is tail recursive and avoids 
;  cycles. 
;  
;  Then design a function called most-followers which determines which user in a Chirper Network is 
;  followed by the most people.
;  



(define-struct user (name verified? follows))
;; User is (make-user String Boolean (listof User))
;; interp. user in Chirper

(define Chirper
  (shared [(-A- (make-user "A" true (list -B-)))
           (-B- (make-user "B" false (list -C- -D-)))
           (-C- (make-user "C" true (list -E-)))
           (-D- (make-user "D" false (list -A- -C-)))
           (-E- (make-user "E" true empty))]
    -A-))

;                   --  D --
;                  /    ^  |
;                 /     |  |
;                /      |  |
;              <'       |  |
;            A -------> B  | 
;                       |  |
;                       |  |
;                       |  |
;                       v <'
;                       C -----------> E



;; template mutually recursive, context acc, worklist
(define (fn-for-chirper c)
  ;; path is (listof String) ; names of users already visited
  (local [(define (fn-for-user path todo user)
            (if (member (user-name user) path)
                (fn-for-lou path todo)
                (fn-for-lou (append path (list (user-name user)))
                            (append todo (user-follows user)))))
          
          (define (fn-for-lou path todo)
            (cond [(empty? todo) (...)]
                  [else
                   (fn-for-user path (rest todo) (first todo))]))]
    (fn-for-user empty empty c)))




;  PROBLEM 2:
;  
;  In UBC's version of How to Code, there are often more than 800 students taking 
;  the course in any given semester, meaning there are often over 40 Teaching Assistants. 
;  
;  Designing a schedule for them by hand is hard work - luckily we've learned enough now to write 
;  a program to do it for us! 
;  
;  Below are some data definitions for a simplified version of a TA schedule. There are some 
;  number of slots that must be filled, each represented by a natural number. Each TA is 
;  available for some of these slots, and has a maximum number of shifts they can work. 
;  
;  Design a search program that consumes a list of TAs and a list of Slots, and produces one
;  valid schedule where each Slot is assigned to a TA, and no TA is working more than their 
;  maximum shifts. If no such schedules exist, produce false. 
; 
;  You should supplement the given check-expects and remember to follow the recipe!



;; Slot is Natural
;; interp. each TA slot has a number, is the same length, and none overlap

(define-struct ta (name max slots))
;; TA is (make-ta String Natural (listof Slot))
;; interp. the TA's name, number of slots they can work, and slots they're available for

(define SOBA (make-ta "Soba" 2 (list 1 3)))
(define UDON (make-ta "Udon" 1 (list 3 4)))
(define RAMEN (make-ta "Ramen" 1 (list 2)))

(define NOODLE-TAs (list SOBA UDON RAMEN))

(define A (make-ta "Erika" 1 (list 1 3 7 9)))
(define B (make-ta "Ryan" 1 (list 1 8 10)))
(define C (make-ta "Reece" 1 (list 5 6)))
(define D (make-ta "Gordon" 2 (list 2 3 9)))
(define E (make-ta "David" 2 (list 2 8 9)))
(define F (make-ta "Katie" 1 (list 4 6)))
(define G (make-ta "Aashlish" 2 (list 1 10)))
(define H (make-ta "Grant" 2 (list 1 11)))
(define I (make-ta "Reannne" 2 (list 1 11 12)))

(define TAs (list A B C D E F G H I))

(define SLOTS (build-list 12 add1))



(define-struct assignment (ta slot))
;; Assignment is (make-assignment TA Slot)
;; interp. the TA is assigned to work the slot

;; Schedule is (listof Assignment)


;; ============================= FUNCTIONS


;; Schedule (listof TA) Natural -> (listof Schedule)
;; list of schedules with an additional assignment of each given TA in the given slot

(define (generate s tas slot)
  (cond [(empty? tas) empty]
        [else
         (cons (cons (make-assignment (first tas) slot) s)
               (generate s (rest tas) slot))]))


;; (listof TA) -> (Schedule -> Boolean)
;; Takes a list of TA and givesa function, which given a schedule, checks its validity against the TAs

(check-expect ((valid? TAs) (list
                             (make-assignment (make-ta "Reannne" 2 (list 1 11 12)) 12)
                             (make-assignment (make-ta "Grant" 2 (list 1 11)) 11)
                             (make-assignment (make-ta "Ryan" 1 (list 1 8 10)) 10)
                             (make-assignment (make-ta "Erika" 1 (list 1 3 7 9)) 9)
                             (make-assignment (make-ta "Ryan" 1 (list 1 8 10)) 8)
                             (make-assignment (make-ta "Erika" 1 (list 1 3 7 9)) 7)
                             (make-assignment (make-ta "Reece" 1 (list 5 6)) 6)
                             (make-assignment (make-ta "Reece" 1 (list 5 6)) 5)
                             (make-assignment (make-ta "Katie" 1 (list 4 6)) 4)
                             (make-assignment (make-ta "Erika" 1 (list 1 3 7 9)) 3)
                             (make-assignment (make-ta "Gordon" 2 (list 2 3 9)) 2)
                             (make-assignment (make-ta "Erika" 1 (list 1 3 7 9)) 1)))
              false)


(check-expect ((valid? TAs) (list
                             (make-assignment (make-ta "Reannne" 2 (list 1 11 12)) 12)
                             (make-assignment (make-ta "Grant" 2 (list 1 11)) 11)
                             (make-assignment (make-ta "Ryan" 1 (list 1 8 10)) 8)
                             (make-assignment (make-ta "Reece" 1 (list 5 6)) 5)
                             (make-assignment (make-ta "Katie" 1 (list 4 6)) 4)
                             (make-assignment (make-ta "Gordon" 2 (list 2 3 9)) 2)
                             (make-assignment (make-ta "Erika" 1 (list 1 3 7 9)) 1)))
              true)

(define (valid? avails)
  (λ (s)
    (cond [(empty? s) true]
          [else
           (local [(define assign (first s))
                   (define slot (assignment-slot assign))
                   (define name (ta-name (assignment-ta assign)))
                   (define avail (find-ta name avails))]

             (cond [(not (member slot (ta-slots avail))) false]
                   [(zero? (ta-max avail)) false]
                   [else
                    ((valid? (remove-avail name
                                           slot
                                           avails)) (rest s))]))])))





(define (find-ta name tas)
  (cond [(empty? tas) false]
        [else
         (if (string=? name (ta-name (first tas)))
             (first tas)
             (find-ta name (rest tas)))]))


;; String Natural (listof TAs) -> (listof TAs)
;; list of TAs given TA's availabilities removed according to given slot

(define (remove-avail name slot tas)
  (if (string=? name (ta-name (first tas)))
      (cons (make-ta name
                     (sub1 (ta-max (first tas)))
                     (remove slot (ta-slots (first tas))))
            (rest tas))
      (cons (first tas)
            (remove-avail name slot (rest tas)))))


(define R1 (remove-avail "Soba" 1 NOODLE-TAs))

(check-expect R1
              (cons (make-ta "Soba" 1 (list 3)) (rest NOODLE-TAs)))

(define R2 (remove-avail "Udon" 4 R1))

(check-expect R2
              (cons (first R1) (cons (make-ta "Udon" 0 (list 3)) (rest (rest R1)))))


;; (listof TA) (listof Slot) -> Schedule or false
;; produce valid schedule given TAs and Slots; false if impossible

(check-expect (schedule-tas empty empty) empty)
(check-expect (schedule-tas empty (list 1 2)) false)
(check-expect (schedule-tas (list SOBA) empty) empty)

(check-expect (schedule-tas (list SOBA) (list 1)) (list (make-assignment SOBA 1)))
(check-expect (schedule-tas (list SOBA) (list 2)) false)
(check-expect (schedule-tas (list SOBA) (list 1 3)) (list (make-assignment SOBA 3)
                                                          (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4)) 
              (list
               (make-assignment UDON 4)
               (make-assignment SOBA 3)
               (make-assignment RAMEN 2)
               (make-assignment SOBA 1)))

(check-expect (schedule-tas NOODLE-TAs (list 1 2 3 4 5)) false)


(define (schedule-tas tas slots)
  (local [(define (fn-for-schedule s tas slots)
            (if (empty? slots)
                s
                (local [(define next (filter (valid? tas)
                                             (generate s
                                                       tas
                                                       (first slots))))]
                  (if (empty? next)
                      false
                      (fn-for-los next
                                  tas
                                  (rest slots))))))

          (define (fn-for-los los tas slots)
            (cond [(empty? los) false]
                  [else
                   (local [(define result (fn-for-schedule (first los)
                                                           tas
                                                           slots))]
                     (if (not (false? result))
                         result
                         (fn-for-los (rest los) tas slots)))]))]
    (fn-for-schedule empty tas slots)))

