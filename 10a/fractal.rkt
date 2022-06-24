
(require 2htdp/image)

;; fractals-starter.rkt

; 
; PROBLEM: 
; 
; Design a function that consumes a number and produces a Sierpinski
; triangle of that size. Your function should use generative recursion.
; 
; One way to draw a Sierpinski triangle is to:
; 
;  - start with an equilateral triangle with side length s
;  
;      .
;      
;  - inside that triangle are three more Sierpinski triangles
;      .
;      
;  - and inside each of those... and so on
;  
; So that you end up with something that looks like this:
;    
; 
;    
; 
; .
;    
; Note that in the 2nd picture above the inner triangles are drawn in 
; black and slightly smaller just to make them clear. In the real
; Sierpinski triangle they should be in the same color and of side
; length s/2. Also note that the center upside down triangle is not
; an explicit triangle, it is simply formed from the other triangles.
; 
; 


(define BASE 2)
;; alternative
(define (tri s)
  (cond [(<= s BASE) (triangle s "outline" "red")]
        [else
         (local [(define sub (tri (/ s 2)))]
           (above sub (beside sub sub)))]))



; 
; PROBLEM:
; 
; Design a function to produce a Sierpinski carpet of size s.
; 
; Here is an example of a larger Sierpinski carpet.
; 
; .
; 
; starting with square, fill it with 8 subsquares with half
; side length in donut arrangement
; 
; (square s "outline" "red")
; (square (/ s 2) "outline" "red")
; 


;; Number -> Image
;; a sierpinski carpet of size given

(check-expect (carpet BASE) (square BASE "outline" "red"))
(check-expect (carpet (* BASE 3))
              (local [(define sub (square BASE "outline" "red"))
                      (define row (beside sub sub sub))
                      (define mid (beside sub (square BASE "solid" "white") sub))]
                (above row mid row)))


;; template gen rec
#;
(define (gen x)
  (if (base? x)
      base
      (... x
           (gen (next x)))))


(define (carpet s)
  (if (<= s BASE)
      (square s "outline" "red")
      (local [(define next (/ s 3))
              (define sub (carpet next))
                      (define row (beside sub sub sub))
                      (define mid (beside sub (square next "solid" "white") sub))]
                (above row mid row))))
      
