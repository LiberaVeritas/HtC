
;; same-house-as-parent-v1.rkt

; 
; PROBLEM:
; 
; In the Harry Potter movies, it is very important which of the four houses a
; wizard is placed in when they are at Hogwarts. This is so important that in 
; most families multiple generations of wizards are all placed in the same family. 
; 
; Design a representation of wizard family trees that includes, for each wizard,
; their name, the house they were placed in at Hogwarts and their children. We
; encourage you to get real information for wizard families from: 
;    http://harrypotter.wikia.com/wiki/Main_Page
; 
; The reason we do this is that designing programs often involves collection
; domain information from a variety of sources and representing it in the program
; as constants of some form. So this problem illustrates a fairly common scenario.
; 
; That said, for reasons having to do entirely with making things fit on the
; screen in later videos, we are going to use the following wizard family tree,
; in which wizards and houses both have 1 letter names. (Sigh)
; 
; 


;; House is one of:
;; "G"
;; "H"
;; "S"
;; "R"
;; interp. HP housees

#;
(define (fn-for-house h)
  (... (cond [(string=? "G" h) (...)]
             [(string=? "H" h) (...)]
             [(string=? "S" h) (...)]
             [(string=? "R" h) (...)])))


(define-struct wiz (name house))
;; Wiz is (make-wiz (String House))
;; interp. a HP wizard

#;
(define (fn-for-wiz w)
  (... (fn-for-string (wiz-name w))
       (fn-for-house (wiz-house w))))


(define-struct tree (wiz kids))
;; Tree is one of:
;; - empty
;; - (make-tree Wiz (listof Tree))
;; interp. a wizard family tree


#;
(define (fn-for-tree t)
  (local [(define (fn-tree t)
            (... (fn-for-wiz (tree-wiz t))
                 (fn-for-lot (tree-kids t))))
          (define (fn-lot lot)
            (cond [(empty? lot) (...)]
                  [else
                   (... (fn-tree (first lot))
                        (fn-lot (rest lot)))]))]
    (fn-tree t)))

(define JP (make-wiz "James Potter" "G"))

(define HP (make-wiz "Harry Potter" "G"))
(define GW (make-wiz "Ginny Weasley" "G"))
(define LLP (make-wiz "Lily Luna Potter" "G"))
(define ASP (make-wiz "Albus Severus Potter" "S"))
(define JSP (make-wiz "James Sirius Potter" "G"))

(define RW (make-wiz "Ron Weasley" "G"))
(define HG (make-wiz "Hermione Granger" "G"))
(define RGW (make-wiz "Rose Granger-Weasley" "G"))

(define T1 (make-tree HP (list (make-tree LLP empty)
                               (make-tree ASP empty)
                               (make-tree JSP empty))))
(define T2 (make-tree JP (list T1)))

; 
; PROBLEM:
; 
; Design a function that consumes a wizard and produces the names of every 
; wizard in the tree that was placed in the same house as their immediate
; parent. 
; 


;; Tree -> (listof String)
;; list of names of every wiz in tree with the same house as root

;; template from Tree
(check-expect (same-house empty) empty)
(check-expect (same-house T2) (list "James Potter"
                                    "Harry Potter"
                                    "Lily Luna Potter"
                                    "James Sirius Potter"))

(define (same-house t)
  (if (empty? t)
      empty
      (map wiz-name
           (filter (house-filter (wiz-house (tree-wiz t)))
                   (list-wiz t)))))



;; House -> (Wiz -> Boolean)
;; function that takes wiz and says true if the wiz is in given house

(define (house-filter h)
  (lambda (w)
    (string=? h (wiz-house w))))

         

;; (listof Wiz) House -> (listof Wiz)
;; list of Wiz with only the ones of given house

(define (only-house h low)
  (filter (lambda (w) (string=? h (wiz-house w))) low))


;; Tree -> (listof Wiz)
;; list of all wiz in tree

(define (list-wiz t)
  (foldt cons append empty t))


;; (Wiz X -> X) ((listof X) -> X) X Tree -> X
;; foldr for tree

(define (foldt fn1 fn2 b t)
  (local [(define (fn-tree t)
            (if (empty? t)
                b
                (fn1 (tree-wiz t)
                     (fn-lot (tree-kids t)))))
          (define (fn-lot lot)
            (cond [(empty? lot) b]
                  [else
                   (fn2 (fn-tree (first lot))
                        (fn-lot (rest lot)))]))]
    (fn-tree t)))


; 
; PROBLEM:
; 
; Design a function that consumes a wizard and produces the number of wizards 
; in that tree (including the root). Your function should be tail recursive.
; 



;; Tree -> Integer
;; number of wizard in tree

(check-expect (count empty) 0)
(check-expect (count T1) 4)
(check-expect (count T2) 5)

(define (count t)
  ;; acc: Integer
  ;; how many we have counted so far
  (local [(define (count2 acc low)
            (cond [(empty? low) acc]
                  [else
                   (count2 (add1 acc) (rest low))]))]
    (count2 0 (list-wiz t))))


; 
; PROBLEM:
; 
; Design a new function definition for same-house-as-parent that is tail 
; recursive. You will need a worklist accumulator.
; 
; 



;; Tree -> (listof Wiz)
;; list of names of every wiz in tree with the same house as root

(check-expect (same-house2 empty) empty)
(check-expect (same-house2 T2) (list "James Potter"
                                     "Harry Potter"
                                     "Lily Luna Potter"
                                     "James Sirius Potter"))

(define (same-house2 t)
  (if (empty? t)
      empty
      
      ;; acc: (listof Wiz)
      ;; current list of result
      (local [(define par (tree-wiz t))
              (define h (wiz-house par))
              (define same? (house-filter h))
              
              (define (same acc low)
                (cond [(empty? low) acc]
                      [else
                       (if (same? (first low))
                           (same (append acc (list (wiz-name (first low))))
                                 (rest low))
                           (same acc
                                 (rest low)))]))]
        (same empty (list-wiz t)))))


