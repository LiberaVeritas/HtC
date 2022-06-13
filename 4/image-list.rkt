(require 2htdp/image)

;; image-list-starter.rkt

;; =================
;; Data definitions:

; 
; PROBLEM A:
; 
; Design a data definition to represent a list of images. Call it ListOfImage. 
; 


;; ListOfImage is one of:
;; - empty
;; - (cons Image ListOfImage)
;; interp. List of Images
(define I0 empty)
(define I1 (cons (square 3 "solid" "red") empty))
(define I2 (cons (circle 3 "solid" "green") (cons (square 3 "solid" "red") empty)))
#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else (... (first loi)
                   (fn-for-loi (rest loi)))]))

;; Template rules:
;; one of: 2 cases
;; - atomic distinct: empty
;; - (cons Image ListOfImage)
;; - self-reference: (rest loi) is ListOfImage

;; =================
;; Functions:

; 
; PROBLEM B:
; 
; Design a function that consumes a list of images and produces a number 
; that is the sum of the areas of each image. For area, just use the image's 
; width times its height.
; 



;; ListOfImage -> Number
;; returns the sum of areas of the the images. Empty is 0
(check-expect (sum-areas empty) 0)
(check-expect (sum-areas (cons (square 4 "solid" "red") empty)) 16)
(check-expect (sum-areas (cons (rectangle 3 4 "solid" "red") (cons (square 4 "solid" "red") empty))) 28)

#;
(define (sum-areas loi) 0) ;stub


;; template copied

(define (sum-areas loi)
  (cond [(empty? loi) 0]
        [else (+ (* (image-width (first loi)) (image-height (first loi)))
                   (sum-areas (rest loi)))]))
