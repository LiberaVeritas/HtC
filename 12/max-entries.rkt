
;; max-exits-to-starter.rkt

; 
; PROBLEM:
; 
; Using the following data definition, design a function that produces the room with the most exits 
; (in the case of a tie you can produce any of the rooms in the tie).
; 


;; Data Definitions: 

(define-struct room (name exits))
;; Room is (make-room String (listof Room))
;; interp. the room's name, and list of rooms that the exits lead to

; .
 
(define H1 (make-room "A" (list (make-room "B" empty))))

; .
 
(define H2 
  (shared ((-0- (make-room "A" (list (make-room "B" (list -0-))))))
    -0-)) 


; .

(define H3
  (shared ((-A- (make-room "A" (list -B-)))
           (-B- (make-room "B" (list -C-)))
           (-C- (make-room "C" (list -A-))))
    -A-))
           


; .

(define H4
  (shared ((-A- (make-room "A" (list -B- -D-)))
           (-B- (make-room "B" (list -C- -E-)))
           (-C- (make-room "C" (list -B-)))
           (-D- (make-room "D" (list -E-)))
           (-E- (make-room "E" (list -F- -A-)))
           (-F- (make-room "F" (list))))
    -A-))

;; template: structural recursion, encapsulate w/ local, tail-recursive w/ worklist, 
;;           context-preserving accumulator what rooms have we already visited



(define-struct entries (room count))
;; (make-entries Room Natural)
;; interp. room and num of entries to it

(define (name=? r1 r2)
  (string=? (room-name r1) (room-name r2)))


;; Room (list of Entries) -> (list of Entries)
;; add one to given room's count

(check-expect (update (make-room "B" empty)
                      (list (make-entries (make-room "X" empty) 1)
                            (make-entries (make-room "B" empty) 2)))
              (list (make-entries (make-room "X" empty) 1)
                    (make-entries (make-room "B" empty) 3)))

(define (update r loe)
  (cond [(empty? loe) empty]
        [else
         (if (name=? r (entries-room (first loe)))
             (cons (make-entries (entries-room (first loe))
                                 (add1 (entries-count (first loe))))
                   (update r (rest loe)))

             (cons (first loe) (update r (rest loe))))]))


;; (listof Entries) -> Room
;; room with max count
#;
(check-expect (max-entries (list (make-entries (make-room "A" empty) 0)
                                 (make-entries (make-room "B" empty) 5)))
              (make-room "B" empty))

(define (max-entries loe)
  (entries-room (foldl (λ (e1 e2)
                         (if (>= (entries-count e1) (entries-count e2))
                             e1
                             e2))
                       (first loe)
                       (rest loe))))

         
(check-expect (max-exits-to H1) (first (room-exits H1)))
(check-expect (max-exits-to H2) (shared ((-A- (make-room "A" (list -B-)))
                                         (-B- (make-room "B" (list -A-))))
                                  -B-))

(check-expect (max-exits-to H4) 
              (shared ((-A- (make-room "A" (list -B- -D-)))
                       (-B- (make-room "B" (list -C- -E-)))
                       (-C- (make-room "C" (list -B-)))
                       (-D- (make-room "D" (list -E-)))
                       (-E- (make-room "E" (list -F- -A-)))
                       (-F- (make-room "F" (list))))
                -E-))  
             

(define (max-exits-to r0)
  ;; todo is (listof Room); a worklist accumulator
  ;; visited is (listof String); context preserving accumulator, names of rooms already visited
  ;; entries
  (local [(define (fn-for-room r todo visited ent)
            (if (member (room-name r) visited)
                (fn-for-lor todo visited (update r ent))
                (fn-for-lor (append (room-exits r) todo)
                            (append visited (list (room-name r)))
                            (append ent (list (make-entries r 1))))))
          (define (fn-for-lor todo visited ent)
            (cond [(empty? todo) (max-entries (rest ent))]
                  [else
                   (fn-for-room (first todo)
                                (rest todo)
                                visited
                                ent)]))]
    (fn-for-room r0 empty empty empty)))
