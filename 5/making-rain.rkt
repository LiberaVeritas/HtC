(require 2htdp/image)
(require 2htdp/universe)

;; making-rain-filtered-starter.rkt

; 
; PROBLEM:
; 
; Design a simple interactive animation of rain falling down a screen. Wherever we click,
; a rain drop should be created and as time goes by it should fall. Over time the drops
; will reach the bottom of the screen and "fall off". You should filter these excess
; drops out of the world state - otherwise your program is continuing to tick and
; and draw them long after they are invisible.
; 
; In your design pay particular attention to the helper rules. In our solution we use
; these rules to split out helpers:
;   - function composition
;   - reference
;   - knowledge domain shift
;   
;   
; NOTE: This is a fairly long problem.  While you should be getting more comfortable with 
; world problems there is still a fair amount of work to do here. Our solution has 9
; functions including main. If you find it is taking you too long then jump ahead to the
; next homework problem and finish this later.
; 
; 


;; Make it rain where we want it to.

;; =================
;; Constants:

(define WIDTH  300)
(define HEIGHT 300)

(define SPEED 1)
(define ACC 0.05)

(define DROP (ellipse 4 8 "solid" "blue"))

(define MTS (rectangle WIDTH HEIGHT "solid" "light blue"))


;; =================
;; Data definitions:

(define-struct drop (x y))
;; Drop is (make-drop Integer Integer)
;; interp. A raindrop on the screen, with x and y coordinates.

(define D1 (make-drop 10 30))

#;
(define (fn-for-drop d)
  (... (drop-x d) 
       (drop-y d)))

;; Template Rules used:
;; - compound: 2 fields


;; ListOfDrop is one of:
;;  - empty
;;  - (cons Drop ListOfDrop)
;; interp. a list of drops

(define LOD0 empty)
(define LOD2 (cons (make-drop 10 20) (cons (make-drop 3 6) empty)))

;; =================
;; Test constants:

(define LOD1 (cons (make-drop 3 6) empty))
(define LOD3 (cons (make-drop 30 40) (cons (make-drop 10 20) (cons (make-drop 3 6) empty))))

#;
(define (fn-for-lod lod)
  (cond [(empty? lod) (...)]
        [else
         (... (fn-for-drop (first lod))
              (fn-for-lod (rest lod)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Drop ListOfDrop)
;; - reference: (first lod) is Drop
;; - self reference: (rest lod) is ListOfDrop

;; =================
;; Functions:

;; ListOfDrop -> ListOfDrop
;; start rain program by evaluating (main empty)
(define (main lod)
  (big-bang lod
            (on-mouse handle-mouse)   ; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
            (on-tick  next-drops)     ; ListOfDrop -> ListOfDrop
            (to-draw  render-drops))) ; ListOfDrop -> Image


;; ListOfDrop Integer Integer MouseEvent -> ListOfDrop
;; if mevt is "button-down" add a new drop at that position
(check-expect (handle-mouse empty 3 6 "button-up") empty)
(check-expect (handle-mouse empty 3 6 "button-down") LOD1)
(check-expect (handle-mouse LOD1 10 20 "button-down") LOD2)
(check-expect (handle-mouse LOD2 30 40 "button-down") LOD3)
#;
(define (handle-mouse lod x y mevt) empty) ; stub

#;
(define (handle-mouse lod x y mevt)
  (cond [(mouse=? mevt "mevt") (fn-for-lod lod x y)]
        [else (... lod x y)]))

(define (handle-mouse lod x y mevt)
    (cond [(mouse=? mevt "button-down")
           (cons (make-drop x y) lod)]
           [else lod]))


;; ListOfDrop -> ListOfDrop
;; produce filtered and ticked list of drops
(check-expect (next-drops empty) empty)
(check-expect (next-drops (cons D1 (cons OFF1 (cons D2 empty))))
              (cons (fall D1) (cons (fall D2) empty)))

#;
(define (next-drops lod)empty) ; stub

(define (next-drops lod)
  (fall-drops (filter lod)))




;; ListOfDrop -> ListOfDrop
;; removed out of screen drops from list, drops whose y is higher than height

;; constants
(define ON1 (make-drop 0 HEIGHT))
(define ON2 (make-drop 0 (- HEIGHT 1)))
(define OFF1 (make-drop 0 (+ HEIGHT 1)))

(define F1 (cons ON1 empty))
(define F2 (cons OFF1 empty))
(define F3 (cons ON1 (cons OFF1 empty)))

(check-expect (filter empty) empty)
(check-expect (filter F2) empty)
(check-expect (filter F3) F1)
#;
(define (filter lod) empty) ;stub

;; template copied

(define (filter lod)
  (cond [(empty? lod) empty]
        [else
         (if (on-screen? (first lod))
             (cons (first lod)
                   (filter (rest lod)))
             (filter (rest lod)))]))

;; Drop -> Boolean
;; returns true if drop is on-screen (y <= HEIGHT)
(check-expect (on-screen? ON1) true)
(check-expect (on-screen? ON2) true)
(check-expect (on-screen? OFF1) false)

#;
(define (on-screen? d) true) ;stub

(define (on-screen? d)
  (<= (drop-y d) HEIGHT))

;; ListOfDrop -> ListOfDrop
;; builds a new list of drops with heights changed by fall
(check-expect (fall-drops empty) empty)
(check-expect (fall-drops (cons D1 (cons D2 empty)))
              (cons (fall D1) (cons (fall D2) empty)))

#;
(define (fall-drops lod) empty) ;stub

;; template copied

(define (fall-drops lod)
  (cond [(empty? lod) empty]
        [else
         (cons (fall (first lod))
               (fall-drops (rest lod)))]))

;; Drop -> Drop
;; changes the height according to speed
;(check-expect (fall D1) (make-drop 10 (+ 30 SPEED)))
;(check-expect (fall D2) (make-drop 0 SPEED))
;(check-expect (fall D3) (make-drop 0 (+ HEIGHT SPEED)))

;; constants

(define D2 (make-drop 0 0))
(define D3 (make-drop 0 HEIGHT))
#;
(define (fall d) D1) ;stub

;; template copied
#;
(define (fall d)
  (make-drop (drop-x d) (+ SPEED (drop-y d))))

;; accelerated fall

(define (fall d)
  (make-drop (drop-x d)
             (+ (* (+ (drop-y d) 1) ACC) (drop-y d))))

;; ListOfDrop -> Image
;; Render the drops onto MTS

#;
(define (render-drops lod) MTS) ; stub

(define (render-drops lod)
  (cond [(empty? lod) MTS]
        [else
         (render (first lod)
                 (render-drops (rest lod)))]))

;; Drop -> Image
;; render a drop

(define (render d s)
  (place-image DROP
               (drop-x d)
               (drop-y d)
               s))
