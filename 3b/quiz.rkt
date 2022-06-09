(require 2htdp/image)
(require 2htdp/universe)

;; A "ship" rectangle that can move around with arrow keys
;; and shoots "bullet" circles with space

;                                                                              
; Constants:                                                                  
; ship image
; bullet image
; width, height, MTS
; ship speed
; bullet speed
; 
; 
; Changing:
; ship position
; bullet position
; bullet existence 
; 
; 
; big bang:
; on-tick
; on-key
; to-draw
;  



;; =================
;; Constants:

(define SHIP (rectangle 60 30 "solid" "green"))
(define BULLET (circle 5 "solid" "red"))
(define SHIP-SPEED 10)
(define BULLET-SPEED 20)
(define WIDTH 800)
(define HEIGHT 800)
(define CTR-X (/ WIDTH 2))
(define CTR-Y (/ HEIGHT 2))
(define MTS (empty-scene WIDTH HEIGHT))

;; =================
;; Data definitions:

(define-struct ship (x y))
;; Ship is (make-ship Natural[0,  WIDTH] Natural[0, HEIGHT])
;; interp. a ship and its position in the scene

(define S1 (make-ship 50 50))
#;
(define (fn-for-ship ship)
  (... (ship-x ship)
       (ship-y ship)))

;; Template rules:
;; compound 2 fields


(define-struct bullet (x y exists))
;; Bullet is (make-bullet Natural[0,  WIDTH] Natural[0, HEIGHT] Boolean)
;; interp. a bullet shot by a ship and its position in the scene when exists is true
;; when bullet reaches top of screen, exists is set to false


(define B1 (make-bullet 50 50 true))
#;
(define (fn-for-bullet ship)
  (... (ship-x ship)
       (ship-y ship)
       (ship-exists ship)))

;; Template ruels:
;; compound 3 fields

(define-struct world (ship bullet))
;; world is (make-world (make-ship x y) (make-bullet x y exists))
;; interp. the world in which a ship and/or bullet lives

(define World1 (make-world (make-ship 0 0) (make-bullet 0 0 true)))
(define World2 (make-world (make-ship 5 5) (make-bullet 5 5 false)))

#;
(define (fn-for-world ship bullet)
  (... (world-ship world)
       (world-bullet world)))

;; Template rules:
;; compound 2 fields

;; =================
;; Functions:

;; World -> World
;; start the world with a centered ship and no bullet
;; 
(define (main w)
  (big-bang w                  ; World
            (on-tick tock)     ; World -> World
            (to-draw render)   ; World -> Image
            (on-key move)))    ; World KeyEvent -> World

;; World -> World
;; moves the bullet if it exists
(check-expect (tock (make-world (make-ship 0 0) (make-bullet 0 0 false)))
              (make-world (make-ship 0 0) (make-bullet 0 0 false)))

(check-expect (tock (make-world (make-ship 0 0) (make-bullet 0 100 true)))
              (make-world (make-ship 0 0) (make-bullet 0 (- 100 BULLET-SPEED) true)))

(check-expect (tock (make-world (make-ship 0 0) (make-bullet 0 0 true)))
              (make-world (make-ship 0 0) (make-bullet 0 0 false)))


#;
(define (tock w)
  (make-world (make-ship 0 0)
              (make-bullet 0 0 false)))

;; template copied

(define (tock w)
  (cond [(false? (bullet-exists (world-bullet w))) w]
        [(< (- (bullet-y (world-bullet w)) BULLET-SPEED) 0)
           (make-world (world-ship w) (make-bullet
                                           (bullet-x (world-bullet w))
                                           0
                                           false))]
        [else
             (make-world (world-ship w) (make-bullet
                                    (bullet-x (world-bullet w))
                                    (- (bullet-y (world-bullet w)) BULLET-SPEED)
                                    true))]))

                                    

;; World -> Image
;; renders the scene with ship and bullet if it exists
(check-expect (render (make-world (make-ship 0 0) (make-bullet 0 0 false)))
              (place-image SHIP 0 0 MTS))

(check-expect (render (make-world (make-ship 0 0) (make-bullet 0 0 true)))
              (place-images
                (list SHIP BULLET)
                (list (make-posn 0 0) (make-posn 0 0))
                MTS))

(check-expect (render (make-world (make-ship 200 400) (make-bullet 50 90 true)))
              (place-images
                (list SHIP BULLET)
                (list (make-posn 200 400) (make-posn 50 90))
                MTS))


;; template copied

#;
(define (render w)
  (place-images
                (list SHIP BULLET)
                (list (make-posn 0 0) (make-posn 0 0))
                MTS))

(define (render w)
  (if (bullet-exists (world-bullet w))
      (place-images (list SHIP BULLET)
                    (list (make-posn (ship-x (world-ship w))
                                     (ship-y (world-ship w)))
                          (make-posn (bullet-x (world-bullet w))
                                     (bullet-y (world-bullet w))))
                    MTS)
      (place-image SHIP (ship-x (world-ship w)) (ship-y (world-ship w)) MTS)))

;; World KeyEvent -> World
;; moves the ship when arrow keys are pressed and shoots bullet if it doesn't already exist
;; tests omitted

(define (move w ke)
  (cond [(and (string=? ke "left") (> (ship-x (world-ship w)) 0))
              (if (>= (ship-x (world-ship w)) SHIP-SPEED)
                  (make-world (make-ship (- (ship-x (world-ship w)) SHIP-SPEED)
                                         (ship-y (world-ship w)))
                              (world-bullet w))
                  (make-world (make-ship 0 (ship-y (world-ship w)))
                              (world-bullet w)))]

        [(and (string=? ke "right") (< (ship-x (world-ship w)) WIDTH))
              (if (<= (ship-x (world-ship w)) (- WIDTH SHIP-SPEED))
                  (make-world (make-ship (+ (ship-x (world-ship w)) SHIP-SPEED)
                                         (ship-y (world-ship w)))
                              (world-bullet w))
                  (make-world (make-ship WIDTH (ship-y (world-ship w)))
                              (world-bullet w)))]

        [(and (string=? ke "up") (> (ship-y (world-ship w)) 0))
              (if (>= (ship-y (world-ship w)) SHIP-SPEED)
                  (make-world (make-ship (ship-x (world-ship w))
                                         (- (ship-y (world-ship w)) SHIP-SPEED))
                              (world-bullet w))
                  (make-world (make-ship (ship-x (world-ship w)) 0 )
                              (world-bullet w)))]

        [(and (string=? ke "down") (< (ship-y (world-ship w)) HEIGHT))
              (if (<= (ship-y (world-ship w)) (- HEIGHT SHIP-SPEED))
                  (make-world (make-ship (ship-x (world-ship w))
                                         (+ (ship-y (world-ship w)) SHIP-SPEED))
                              (world-bullet w))
                  (make-world (make-ship (ship-x (world-ship w)) 0 )
                              (world-bullet w)))]
        [(and (string=? ke " ") (false? (bullet-exists (world-bullet w))))
              (make-world (world-ship w) (make-bullet (ship-x (world-ship w))
                                                    (ship-y (world-ship w))
                                                    true))]
        [else w]))


              
