
;; accounts-starter.rkt

(define-struct node (id name bal l r))
;; Accounts is one of:
;;  - false
;;  - (make-node Natural String Integer Accounts Accounts)
;; interp. a collection of bank accounts
;;   false represents an empty collection of accounts.
;;   (make-node id name bal l r) is a non-empty collection of accounts such that:
;;    - id is an account identification number (and BST key)
;;    - name is the account holder's name
;;    - bal is the account balance in dollars CAD 
;;    - l and r are further collections of accounts
;; INVARIANT: for a given node:
;;     id is > all ids in its l(eft)  child
;;     id is < all ids in its r(ight) child
;;     the same id never appears twice in the collection

(define ACT0 false)
(define ACT1 (make-node 1 "Mr. Rogers"  22 false false))
(define ACT4 (make-node 4 "Mrs. Doubtfire"  -3
                        false
                        (make-node 7 "Mr. Natural" 13 false false)))
(define ACT3 (make-node 3 "Miss Marple"  600 ACT1 ACT4))
(define ACT42 
  (make-node 42 "Mr. Mom" -79
             (make-node 27 "Mr. Selatcia" 40 
                        (make-node 14 "Mr. Impossible" -9 false false)
                        false)
             (make-node 50 "Miss 604"  16 false false)))
(define ACT10 (make-node 10 "Dr. No" 84 ACT3 ACT42))

#;
(define (fn-for-act act)
  (cond [(false? act) (...)]
        [else
         (... (node-id act)
              (node-name act)
              (node-bal act)
              (fn-for-act (node-l act))
              (fn-for-act (node-r act)))]))


; PROBLEM 1:
; 
; Design an abstract function (including signature, purpose, and tests) 
; to simplify the remove-debtors and remove-profs functions defined below.
; 
; Now re-define the original remove-debtors and remove-profs functions 
; to use your abstract function. Remember, the signature and tests should 
; not change from the original functions.


;; Accounts -> Accounts
;; remove all accounts with a negative balance
(check-expect (remove-debtors (make-node 1 "Mr. Rogers" 22 false false)) 
              (make-node 1 "Mr. Rogers" 22 false false))

(check-expect (remove-debtors (make-node 14 "Mr. Impossible" -9 false false))
              false)

(check-expect (remove-debtors
               (make-node 27 "Mr. Selatcia" 40
                          (make-node 14 "Mr. Impossible" -9 false false)
                          false))
              (make-node 27 "Mr. Selatcia" 40 false false))

(check-expect (remove-debtors 
               (make-node 4 "Mrs. Doubtfire" -3
                          false 
                          (make-node 7 "Mr. Natural" 13 false false)))
              (make-node 7 "Mr. Natural" 13 false false))


(define (remove-debtors act)
  (local [(define (negative? x)
            (> (node-bal x) 0))]
    (filter-accounts negative? act)))

#;
(define (remove-debtors act)
  (cond [(false? act) false]
        [else
         (if (negative? (node-bal act))
             (join (remove-debtors (node-l act))
                   (remove-debtors (node-r act)))
             (make-node (node-id act)
                        (node-name act)
                        (node-bal act)
                        (remove-debtors (node-l act))
                        (remove-debtors (node-r act))))]))


;; Accounts -> Accounts
;; Remove all professors' accounts.  
(check-expect (remove-profs (make-node 27 "Mr. Smith" 100000 false false)) 
              (make-node 27 "Mr. Smith" 100000 false false))
(check-expect (remove-profs (make-node 44 "Prof. Longhair" 2 false false)) false)
(check-expect (remove-profs (make-node 67 "Mrs. Dash" 3000
                                       (make-node 9 "Prof. Booty" -60 false false)
                                       false))
              (make-node 67 "Mrs. Dash" 3000 false false))
(check-expect (remove-profs 
               (make-node 97 "Prof. X" 7
                          false 
                          (make-node 112 "Ms. Magazine" 467 false false)))
              (make-node 112 "Ms. Magazine" 467 false false))

(define (remove-profs act)
  (local [(define (not-prof? x)
            (not (has-prefix? "Prof." (node-name x))))]
    (filter-accounts not-prof? act)))

#;
(define (remove-profs act)
  (cond [(false? act) false]
        [else
         (if (has-prefix? "Prof." (node-name act))
             (join (remove-profs (node-l act))
                   (remove-profs (node-r act)))
             (make-node (node-id act)
                        (node-name act)
                        (node-bal act)
                        (remove-profs (node-l act))
                        (remove-profs (node-r act))))]))



;; (Accounts -> Boolean) Accounts -> Accounts
;; filters accounts that satisfy a predicate

(define (even-id? a)
  (even? (node-id a)))

(check-expect (filter-accounts even-id? ACT0) false)
(check-expect (filter-accounts even-id? ACT1) false)
(check-expect (filter-accounts even-id? ACT4) (make-node 4 "Mrs. Doubtfire"  -3 false false))
(check-expect (filter-accounts even-id? ACT42)
              (make-node 42 "Mr. Mom" -79 (make-node 14 "Mr. Impossible" -9 false false)
                         (make-node 50 "Miss 604"  16 false false)))

(define (filter-accounts p a)
  (cond [(false? a) false]
        [else 
         (local [(define meets? (p a))]
           (if meets?
               (make-node (node-id a)
                          (node-name a)
                          (node-bal a)
                          (filter-accounts p (node-l a))
                          (filter-accounts p (node-r a)))
               (join (filter-accounts p (node-l a))
                     (filter-accounts p (node-r a)))))]))


;; String String -> Boolean
;; Determine whether pre is a prefix of str.
(check-expect (has-prefix? "" "rock") true)
(check-expect (has-prefix? "rock" "rockabilly") true)
(check-expect (has-prefix? "blues" "rhythm and blues") false)

(define (has-prefix? pre str)
  (string=? pre (substring str 0 (string-length pre))))

;; Accounts Accounts -> Accounts
;; Combine two Accounts's into one
;; ASSUMPTION: all ids in act1 are less than the ids in act2
(check-expect (join ACT42 false) ACT42)
(check-expect (join false ACT42) ACT42)
(check-expect (join ACT1 ACT4) 
              (make-node 4 "Mrs. Doubtfire" -3
                         ACT1
                         (make-node 7 "Mr. Natural" 13 false false)))
(check-expect (join ACT3 ACT42) 
              (make-node 42 "Mr. Mom" -79
                         (make-node 27 "Mr. Selatcia" 40
                                    (make-node 14 "Mr. Impossible" -9
                                               ACT3
                                               false)
                                    false)
                         (make-node 50 "Miss 604" 16 false false)))

(define (join act1 act2)
  (cond [(false? act2) act1]
        [else
         (make-node (node-id act2) 
                    (node-name act2)
                    (node-bal act2)
                    (join act1 (node-l act2))
                    (node-r act2))]))


; PROBLEM 2:
; 
; Using your new abstract function, design a function that removes from a given
; BST any account where the name of the account holder has an odd number of
; characters.  Call it remove-odd-characters.


;; Accounts -> Accounts
;; returns accounts with odd-number names removed

(check-expect (remove-odd-characters false) false)
(check-expect (remove-odd-characters ACT1) ACT1)
(check-expect (remove-odd-characters ACT4) (make-node 4 "Mrs. Doubtfire"  -3 false false))
(check-expect (remove-odd-characters ACT3) (make-node 4 "Mrs. Doubtfire"  -3 ACT1 false))
(check-expect (remove-odd-characters ACT42) (make-node 50 "Miss 604"  16
                                                       (make-node 27 "Mr. Selatcia" 40
                                                                  (make-node 14 "Mr. Impossible" -9 false false)
                                                                  false)
                                                       false))

(define (remove-odd-characters a)
  (local [(define (not-odd? x)
            (even? (string-length (node-name x))))]
    (filter-accounts not-odd? a)))


; Problem 3:
; 
; Design an abstract fold function for Accounts called fold-act. 
; 
; Use fold-act to design a function called charge-fee that decrements
; the balance of every account in a given collection by the monthly fee of 3 CAD.


;; (Accounts X X -> X) X Accounts -> X
;; fold for accounts

(define (sum-bal a)
  (local [(define (sum-a a x y)
            (+ (node-bal a)
               x y))]
    (fold-act sum-a 0 a)))


(check-expect (sum-bal ACT1) 22)
(check-expect (sum-bal ACT4) 10)
(check-expect (sum-bal ACT42) -32)

(define (fold-act fn base act)
  (cond [(false? act) base]
        [else
         (fn act
             (fold-act fn base (node-l act))
             (fold-act fn base (node-r act)))]))


;; Accounts -> Accounts
;; reduces all balances by 3

(check-expect (minus3 false) false)
(check-expect (minus3 ACT1) (make-node 1 "Mr. Rogers"  19 false false))
(check-expect (minus3 ACT3) (make-node 3 "Miss Marple"  597
                                       (make-node 1 "Mr. Rogers"  19 false false)
                                       (make-node 4 "Mrs. Doubtfire"  -6
                                                  false
                                                  (make-node 7 "Mr. Natural" 10 false false))))


(define (minus3 a)
  (local [(define (deduct a x y)
            (make-node (node-id a)
                       (node-name a)
                       (- (node-bal a) 3)
                       x
                       y))]
    (fold-act deduct false a)))

; PROBLEM 4:
; 
; Suppose you needed to design a function to look up an account based on its ID.
; Would it be better to design the function using fold-act, or to design the
; function using the fn-for-acts template?  Briefly justify your answer.


;; fold doesn't allow for short circuiting and evaluate everything
