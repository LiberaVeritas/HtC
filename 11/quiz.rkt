;  PROBLEM 1:
;  
;  Assuming the use of at least one accumulator, design a function that consumes a list of strings,
;  and produces the length of the longest string in the list. 
;  



;; (listof String) -> Natural
;; length of longest string in list

(check-expect (longest empty) 0)
(check-expect (longest (list "a" "b" "c")) 1)
(check-expect (longest (list "a" "b" "cd")) 2)
(check-expect (longest (list "a" "bdf" "cd")) 3)


(define (longest los)
  ; acc is Natural
  ; length of longest string so far
  (local [(define (fn acc los)
            (cond [(empty? los) acc]
                  [else
                   (if (> (string-length (first los))
                          acc)
                       (fn (string-length (first los)) (rest los))
                       (fn acc (rest los)))]))]
    (fn 0 los)))


;  PROBLEM 2:
;  
;  The Fibbonacci Sequence https://en.wikipedia.org/wiki/Fibonacci_number is 
;  the sequence 0, 1, 1, 2, 3, 5, 8, 13,... where the nth element is equal to 
;  n-2 + n-1. 
;  
;  Design a function that given a list of numbers at least two elements long, 
;  determines if the list obeys the fibonacci rule, n-2 + n-1 = n, for every 
;  element in the list. The sequence does not have to start at zero, so for 
;  example, the sequence 4, 5, 9, 14, 23 would follow the rule. 
;  



;; assume lon is at least 2 elements

;; (list of Number) -> Boolean
;; whether list obeys fibonacci rule
;; ie. each number is the sum of the two previous

(check-expect (fib? (list 1 2)) true)
(check-expect (fib? (list 4 5 9 14 23)) true)
(check-expect (fib? (list 4 5 9 13 23)) false)


(define (fib? lon)
  ;; n-2 is Number
  ;; n-2th fib seq
  ;; n-1 is Number
  ;; n-1th fib seq
  (local [(define (fn n-2 n-1 lon)
            (cond [(empty? lon) true]
                  [else
                   (if (= (+ n-2 n-1) (first lon))
                       (fn n-1 (first lon) (rest lon))
                       false)]))]
    (fn (first lon) (second lon) (rest (rest lon)))))



;  PROBLEM 3:
;  
;  Refactor the function below to make it tail recursive.  
;  


;; Natural -> Natural
;; produces the factorial of the given number
(check-expect (fact 0) 1)
(check-expect (fact 3) 6)
(check-expect (fact 5) 120)

#;
(define (fact n)
  (cond [(zero? n) 1]
        [else 
         (* n (fact (sub1 n)))]))


(define (fact n)
  ;; p is Number
  ;; product so far
  (local [(define (fn p n)
            (cond [(zero? n) p]
                  [else 
                   (fn (* p n) (sub1 n))]))]
    (fn 1 n)))


;  PROBLEM 4:
;  
;  Recall the data definition for Region from the Abstraction Quiz. Use a worklist 
;  accumulator to design a tail recursive function that counts the number of regions 
;  within and including a given region. 
;  So (count-regions CANADA) should produce 7



(define-struct region (name type subregions))
;; Region is (make-region String Type (listof Region))
;; interp. a geographical region

;; Type is one of:
;; - "Continent"
;; - "Country"
;; - "Province"
;; - "State"
;; - "City"
;; interp. categories of geographical regions

(define VANCOUVER (make-region "Vancouver" "City" empty))
(define VICTORIA (make-region "Victoria" "City" empty))
(define BC (make-region "British Columbia" "Province" (list VANCOUVER VICTORIA)))
(define CALGARY (make-region "Calgary" "City" empty))
(define EDMONTON (make-region "Edmonton" "City" empty))
(define ALBERTA (make-region "Alberta" "Province" (list CALGARY EDMONTON)))
(define CANADA (make-region "Canada" "Country" (list BC ALBERTA)))

#;
(define (fn-for-region r)
  (local [(define (fn-for-region r)
            (... (region-name r)
                 (fn-for-type (region-type r))
                 (fn-for-lor (region-subregions r))))
          
          (define (fn-for-type t)
            (cond [(string=? t "Continent") (...)]
                  [(string=? t "Country") (...)]
                  [(string=? t "Province") (...)]
                  [(string=? t "State") (...)]
                  [(string=? t "City") (...)]))
          
          (define (fn-for-lor lor)
            (cond [(empty? lor) (...)]
                  [else 
                   (... (fn-for-region (first lor))
                        (fn-for-lor (rest lor)))]))]
    (fn-for-region r)))

;; Region -> Number
;; number of regions within given, including given
(check-expect (count-regions VANCOUVER) 1)
(check-expect (count-regions CANADA) 7)

(define (count-regions r)
  ;; rsf is counted so far
  ;; todo is (list of Region) to count
  (local [(define (fn-for-region rsf todo r)
            (fn-for-lor (add1 rsf)
                        (append (region-subregions r)
                                todo)))
          
          (define (fn-for-lor rsf todo)
            (cond [(empty? todo) rsf]
                  [else 
                   (fn-for-region rsf (rest todo) (first todo))]))]
    (fn-for-region 0 empty r)))

