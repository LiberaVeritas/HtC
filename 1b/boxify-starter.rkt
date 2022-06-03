
PROBLEM:

Use the How to Design Functions (HtDF) recipe to design a function that consumes an image, 
and appears to put a box around it. Note that you can do this by creating an "outline" 
rectangle that is bigger than the image, and then using overlay to put it on top of the image. 
For example:

(boxify (ellipse 60 30 "solid" "red")) should produce .

Remember, when we say DESIGN, we mean follow the recipe.

Leave behind commented out versions of the stub and template.
(require 2htdp/image)

;; boxify-starter.rkt

; 
; PROBLEM:
; 
; Use the How to Design Functions (HtDF) recipe to design a function that consumes an image, 
; and appears to put a box around it. Note that you can do this by creating an "outline" 
; rectangle that is bigger than the image, and then using overlay to put it on top of the image. 
; For example:
; 
; (boxify (ellipse 60 30 "solid" "red")) should produce .
; 
; Remember, when we say DESIGN, we mean follow the recipe.
; 
; Leave behind commented out versions of the stub and template.
; 


;; Image -> Image
;; Takes an image and returns a new image where there is a box around the original image

; (define (boxify img) .)   ;stub

(check-expect (boxify (ellipse 60 30 "solid" "red")) .)

;(define (boxify img)
;  (...img))

(define (boxify img)
  (overlay img
           (rectangle
             (image-width img)
             (image-height img)
             "outline"
             "black")))
