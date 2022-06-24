(require 2htdp/image)

;; fold-dir-starter.rkt

; 
; In this exercise you will be need to remember the following DDs 
; for an image organizer.
; 


;; =================
;; Data definitions:

(define-struct dir (name sub-dirs images))
;; Dir is (make-dir String ListOfDir ListOfImage)
;; interp. An directory in the organizer, with a name, a list
;;         of sub-dirs and a list of images.

;; ListOfDir is one of:
;;  - empty
;;  - (cons Dir ListOfDir)
;; interp. A list of directories, this represents the sub-directories of
;;         a directory.

;; ListOfImage is one of:
;;  - empty
;;  - (cons Image ListOfImage)
;; interp. a list of images, this represents the sub-images of a directory.
;; NOTE: Image is a primitive type, but ListOfImage is not.

(define I1 (square 10 "solid" "red"))
(define I2 (square 12 "solid" "green"))
(define I3 (rectangle 13 14 "solid" "blue"))
(define D4 (make-dir "D4" empty (list I1 I2)))
(define D5 (make-dir "D5" empty (list I3)))
(define D6 (make-dir "D6" (list D4 D5) empty))

(define D7 (make-dir "D7" (list D6 D4 D5) empty))

;; =================
;; Functions:

; 
; PROBLEM A:
; 
; Design an abstract fold function for Dir called fold-dir. 
; 


;; (Dir (listof X) -> X) X Dir -> X
;; fold for Dir

(check-expect (fold-dir id empty D4) D4)
(check-expect (fold-dir id empty D5) D5)
(check-expect (fold-dir id empty D6) D6)
(check-expect (fold-dir id empty D7) D7)

(define (id d lox)
  (make-dir (dir-name d)
            lox
            (dir-images d)))

(define (fold-dir fn base d)
  (local [(define (fold-lod lod)
            (cond [(empty? lod) empty]
                  [else
                   (cons (fold-dir fn base (first lod))
                         (fold-lod (rest lod)))]))]
    (fn d (fold-lod (dir-sub-dirs d)))))





; 
; PROBLEM B:
; 
; Design a function that consumes a Dir and produces the number of 
; images in the directory and its sub-directories. 
; Use the fold-dir abstract function.
; 


;; Dir -> Natural
;; returns the total number of images in the dir and subdirs

(check-expect (num-images D4) 2)
(check-expect (num-images D5) 1)
(check-expect (num-images D6) 3)
(check-expect (num-images D7) 6)

(define (num-images d)
  (local [(define (fn d lox)
            (+ (length (dir-images d))
               (foldr + 0 lox)))]
    (fold-dir fn 0 d)))


; 
; PROBLEM C:
; 
; Design a function that consumes a Dir and a String. The function looks in
; dir and all its sub-directories for a directory with the given name. If it
; finds such a directory it should produce true, if not it should produce false. 
; Use the fold-dir abstract function.
; 


;; Dir String -> Boolean
;; finds a dir with name matching given string, or false

(check-expect (find D4 "D5") false)
(check-expect (find D4 "D4") true)
(check-expect (find D6 "D5") true)
(check-expect (find D6 "D3") false)


(define (true? x)
  (not (false? x)))

(define (find d s)
  (local [(define (fn d lox)
            (if (string=? (dir-name d) s)
                true
                (ormap true? lox)))]
    (fold-dir fn false d)))

; 
; PROBLEM D:
; 
; Is fold-dir really the best way to code the function from part C? Why or 
; why not?
; 


; no because no shortcutting and evaluating all cases


