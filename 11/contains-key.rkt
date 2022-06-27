
;; bt-contains-tr-starter.rkt

; Problem:
; 
; Starting with the following data definition for a binary tree (not a binary search tree) 
; design a tail-recursive function called contains? that consumes a key and a binary tree 
; and produces true if the tree contains the key.
; 


(define-struct node (k v l r))
;; BT is one of:
;;  - false
;;  - (make-node Integer String BT BT)
;; Interp. A binary tree, each node has a key, value and 2 children
(define BT1 false)
(define BT2 (make-node 1 "a"
                       (make-node 6 "f"
                                  (make-node 4 "d" false false)
                                  false)
                       (make-node 7 "g" false false)))



;; BT Integer -> Boolean
;;  true if the tree contains the given key

(check-expect (contains? BT1 4) false)
(check-expect (contains? BT2 4) true)
(check-expect (contains? BT2 5) false)

(define (contains? bt k)
  ;; acc: todo is (listof BT)
  ;; to revisit
  (local [(define (fn todo)
            (cond [(empty? todo) false]
                  [(false? (first todo)) (fn (rest todo))]
                  [else
                   (if (= k (node-k (first todo)))
                       true
                       (fn (append (rest todo)
                                   (list (node-l (first todo)))
                                   (list (node-r (first todo))))))]))]
    (fn (list bt))))
