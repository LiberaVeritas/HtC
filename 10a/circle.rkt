(require 2htdp/image)

;; circle-fractal-starter.rkt

; 
; PROBLEM :
; 
; Design a function that will create the following fractal:
; 
;             .
; 
;             
; 
; Each circle is surrounded by circles that are two-fifths smaller. 
; 
; You can build these images using the convenient beside and above functions
; if you make your actual recursive function be one that just produces the
; top leaf shape. You can then rotate that to produce the other three shapes.
; 
; You don't have to use this structure if you are prepared to use more
; complex place-image functions and do some arithmetic. But the approach
; where you use the helper is simpler.
; 
; Include a termination argument for your design.


;; =================
;; Constants:

(define STEP (/ 2 5))
(define TRIVIAL-SIZE 5)
(define BASE TRIVIAL-SIZE)




;; Integer[> 0] -> Image
;; circle fractal

(define C1 (/ (* 5 BASE) 2))

(check-expect (fractal BASE) (circle BASE "solid" "blue"))
(check-expect (fractal C1)
              (local [(define next-circ (circle BASE "solid" "blue"))]
                (above next-circ
                       (above (beside next-circ
                                      (circle C1 "solid" "blue")
                                      next-circ)
                              next-circ))))
                         

(define (fractal r)
  (local [(define (left i)
            (rotate 90 i))

          (define (right i)
            (rotate 270 i))

          (define (bottom i)
            (rotate 180 i))

          (define (next x)
            (* x STEP))

          (define (circ r)
            (circle r "solid" "blue"))
        
          (define (helper x) ; fractal on top only
            (if (<= x BASE)
                (circ x)
                (local [(define next-circ (helper (next x)))]
                  (above next-circ
                         (beside (left next-circ)
                                 (circle x "solid" "blue")
                                 (right next-circ))))))

          (define t (helper (next r)))]
    (if (<= r BASE)
        (circ r)
        (above t
               (above (beside (left t)
                              (circ r)
                              (right t))
                      (bottom t))))))


