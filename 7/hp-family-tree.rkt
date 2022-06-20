
;; hp-family-tree-starter.rkt

; In this problem set you will represent information about descendant family 
; trees from Harry Potter and design functions that operate on those trees.
; 
; To make your task much easier we suggest two things:
;   - you only need a DESCENDANT family tree
;   - read through this entire problem set carefully to see what information 
;     the functions below are going to need. Design your data definitions to
;     only represent that information.
;   - you can find all the information you need by looking at the individual 
;     character pages like the one we point you to for Arthur Weasley.
; 


; PROBLEM 1:
; 
; Design a data definition that represents a family tree from the Harry Potter 
; wiki, which contains all necessary information for the other problems.  You 
; will use this data definition throughout the rest of the homework.
; 



(define-struct person (name wand patronus children))
;; Person is (make-person String ListOfPerson)
;; interp. a person with a name, wood type of their wand, their patronus, and their children
;; "" is unknown
;; multiple wands is given with a comma separated list

;; ListOfPerson is one of:
;; - empty
;; - (cons Person ListOfPerson)
;; interp. a list of Person, eg. descendants


(define JAMES (make-person "James" "" "" empty))
(define ALBUS (make-person "Albus" "" "" empty))
(define LILY (make-person "Lily" "" "" empty))

(define CHARLIE (make-person "Charlie" "Ash, Unknown" "Non-corporeal" empty))
(define PERCY (make-person "Percy" "" "Non-corporeal" empty))
(define FRED (make-person "Fred" "" "Magpie" empty))
(define GEORGE (make-person "George" "" "Magpie" empty))
(define RON (make-person "Ron" "Ash, Willow, Chestnut" "Jack Russel terrier" empty))

(define LOP3 (list JAMES ALBUS LILY))
(define GINNY (make-person "Ginny" "Yew" "Horse" LOP3))
(define VICTOIRE (make-person "Victoire" "" "" empty))
(define LOP2 (list VICTOIRE))
(define BILL (make-person "Bill" "" "Non-corporeal" LOP2))
(define LOP1 (list BILL CHARLIE PERCY FRED GEORGE RON GINNY))
#;
(define ARTHUR (make-person "Arthur" "" "Weasel" LOP1))


(define (fn-for-person p)
  (... (person-name p)
       (fn-for-lop (person-children p))))


(define (fn-for-lop lop)
  (cond [(empty? lop) (...)]
        [else
         (... (fn-for-person (first lop))
              (fn-for-lop (rest lop)))]))


; PROBLEM 2: 
; 
; Define a constant named ARTHUR that represents the descendant family tree for 
; Arthur Weasley. You can find all the infomation you need by starting 
; at: http://harrypotter.wikia.com/wiki/Arthur_Weasley.
; 
; You must include all of Arthur's children and these grandchildren: Lily, 
; Victoire, Albus, James.
; 
; 
; Note that on the Potter wiki you will find a lot of information. But for some 
; people some of the information may be missing. Enter that information with a 
; special value of "" (the empty string) meaning it is not present. Don't forget
; this special value when writing your interp.
; 


(define ARTHUR (make-person "Arthur" "" "Weasel" LOP1))


; PROBLEM 3:
; 
; Design a function that produces a pair list (i.e. list of two-element lists)
; of every person in the tree and his or her patronus. For example, assuming 
; that HARRY is a tree representing Harry Potter and that he has no children
; (even though we know he does) the result would be: (list (list "Harry" "Stag")).
; 
; You must use ARTHUR as one of your examples.
; 


;; Person -> ListOfString
;; given a person, makes a name patronus pair

(check-expect (pair GINNY) (list "Ginny" "Horse"))
(check-expect (pair LILY) (list "Lily" ""))

;(define (pair p) empty)

;; template from Person

(define (pair p)
  (list (person-name p) (person-patronus p)))


;; Person -> ListOfPair
;; produces a list of name patronus pairs for the whole tree

; (list BILL CHARLIE PERCY FRED GEORGE RON GINNY)

(check-expect (pair-tree--person ARTHUR) (list (pair ARTHUR)
                                               (pair BILL) (pair VICTOIRE)
                                               (pair CHARLIE)
                                               (pair PERCY)
                                               (pair FRED)
                                               (pair GEORGE)
                                               (pair RON)
                                               (pair GINNY)
                                               (pair JAMES)
                                               (pair ALBUS)
                                               (pair LILY)))





;(define (pair-tree p) empty)

;; template from ListOfPerson

(define (pair-tree--person p)
  (cons (list (person-name p) (person-patronus p))
        (pair-tree--lop (person-children p))))


(define (pair-tree--lop lop)
  (cond [(empty? lop) empty]
        [else
         (append (pair-tree--person (first lop))
                 (pair-tree--lop (rest lop)))]))


; PROBLEM 4:
; 
; Design a function that produces the names of every person in a given tree 
; whose wands are made of a given material. 
; 
; You must use ARTHUR as one of your examples.
; 



;; Person String -> Boolean
;; returns true if the person's wand material list contains the given one
;; "" matches all

(check-expect (wand? GINNY "Yew") true)
(check-expect (wand? GINNY "Ash") false)
(check-expect (wand? ARTHUR "Ash") false)
(check-expect (wand? GINNY "") true)

#;
(define (wand? p w) false)

(define (wand? p w)
  (string-contains? w (person-wand p)))


;; Person String -> ListOfString
;; returns a list of names who have a given type of wand in the whole tree from given person

; (list BILL CHARLIE PERCY FRED GEORGE RON GINNY)

(check-expect (wands--person ARTHUR "Ash") (list "Charlie" "Ron"))
(check-expect (wands--person ARTHUR "Yew") (list "Ginny"))
(check-expect (wands--person ARTHUR "Willow") (list "Ron"))


;(define (wands--person p w) empty)

;; template from ListOfPerson

(define (wands--person p w)
  (if (wand? p w)
      (cons (person-name p)
            (wands--lop (person-children p) w))
      (wands--lop (person-children p) w)))


(define (wands--lop lop w)
  (cond [(empty? lop) empty]
        [else
         (append (wands--person (first lop) w)
                 (wands--lop (rest lop) w))]))
