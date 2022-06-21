
;; merge-starter.rkt

; Problem:
; 
; Design the function merge. It consumes two lists of numbers, which it assumes are 
; each sorted in ascending order. It produces a single list of all the numbers, 
; also sorted in ascending order. 
; 
; Your solution should explicitly show the cross product of type comments table, 
; filled in with the values in each case. Your final function should have a cond 
; with 3 cases. You can do this simplification using the cross product table by 
; recognizing that there are subtly equal answers. 
; 
; Hint: Think carefully about the values of both lists. You might see a way to 
; change a cell content so that 2 cells have the same value.
; 


;; ListOfNumber ListOfNumber -> ListOfNumber
;; takes two sorted lists and merges them together into one sorted list


;; CROSS PRODUCT OF TYPE COMMENTS TABLE
;;
;;                                       lstb
;;                              empty              (cons Number LON)                
;;                                             |
;; l   empty                    empty          |  (append lsta lstb)
;; s                         -----------------------------------------
;; t   (cons Number LON)   (append lsta lstb)  |    (if (< (first lsta) (first lstb))
;; a                                           |        (cons (first lsta)
;;                                             |              (merge (rest lsta) lstb))
;;                                             |        (cons (first lstb)
;;                                             |              (merge lsta (rest lstb))))]))

(check-expect (merge (list 1 2 3 4) (list 5 6 7 8)) (list 1 2 3 4 5 6 7 8))
(check-expect (merge (list 3 5 7 9) (list 4 6 8 10)) (list 3 4 5 6 7 8 9 10))
(check-expect (merge (list 1 2) empty) (list 1 2))
(check-expect (merge empty (list 1 2)) (list 1 2))

(define (merge lsta lstb)
  (cond [(or (empty? lsta) (empty? lstb)) (append lsta lstb)]
        [else
         (if (< (first lsta) (first lstb))
             (cons (first lsta)
                   (merge (rest lsta) lstb))
             (cons (first lstb)
                   (merge lsta (rest lstb))))]))
