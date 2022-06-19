(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 2)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 2)
(define TANK-SPEED 3)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 10)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-Y (- HEIGHT TANK-HEIGHT/2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))






;; Game -> Game
;; Start with a tank at center with no bullets or invaders

(define (main g)
  (big-bang g
    (on-tick adv-game)  ; Game -> Game
    (to-draw draw-game) ; Game -> Image
    (on-key handle-key) ; Game KeyEvent -> Game
    (stop-when game-over?))) ; Game -> Boolean


;; Game -> Game
;; advance game state by moving and removing objects as required
#;
(define (adv-game g) G0)


;; Game -> Game
;; produces the next game state

(define (adv-game g)
  (adv-game-elements (only-surviving g)))


;; Game -> Game
;; produces the next state of each of the game elements
;; by removing topped missiles and hit invaders

(define (adv-game-elements g)
  (make-game (adv-invaders (spawn-invaders (game-invaders g)))
             (adv-missiles (cull-missiles (game-missiles g)))
             (adv-tank (game-tank g))))



;; ListOfInvader -> ListOfInvader
;; spawns an invader randomly and returns new list of invaders

(define (spawn-invaders loi)
  (if (< (random 100) INVADE-RATE)
      (cons (make-invader (random WIDTH) 0 (* (- (random 3) 1) INVADER-X-SPEED)) loi)
      loi))


;; Game -> Game
;; returns game with hit missiles and invaders removed

(define (only-surviving g)
  (make-game (surviving-invaders (game-invaders g) (game-missiles g))
             (surviving-missiles (game-invaders g) (game-missiles g))
             (game-tank g)))



;; ListOfInvader ListOfMissiles -> ListOfInvaders
;; returns only the survivig invaders

(define (surviving-invaders loi lom)
  (cond [(or (empty? loi) (empty? lom)) loi]
        [else
         (if (hit-missiles? (first loi) lom)
             (surviving-invaders (rest loi) lom)
             (cons (first loi)
                   (surviving-invaders (rest loi) lom)))]))


;; ListOfInvader ListOfMissiles -> ListOfMissiles
;; returns only the survivig missiles

(define (surviving-missiles loi lom)
  (cond [(or (empty? loi) (empty? lom)) lom]
        [else
         (if (hit-invaders? loi (first lom))
             (surviving-missiles loi (rest lom))
             (cons (first lom)
                   (surviving-missiles loi (rest lom))))]))



;; ListOfInvader -> ListOfInvader
;; moves each invader

(define (adv-invaders loi)
  (cond [(empty? loi) empty]
        [else (cons (move-invader (first loi))
                    (adv-invaders (rest loi)))]))


;; Invader -> Invader
;; moves invader on the scene

(define (move-invader i)
  (cond [(and (> (invader-dx i) 0) (over-width? i))
         (make-invader (- WIDTH (over-width i))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (* -1 (invader-dx i)))]
        [(and (< (invader-dx i) 0) (less-0? i))
         (make-invader (less-0 i)
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (* -1 (invader-dx i)))]
        [else
         (make-invader (+ (invader-x i) (invader-dx i))
                       (+ (invader-y i) INVADER-Y-SPEED)
                       (invader-dx i))]))


;; Invader -> Boolean
;; returns true if invader's next x is beyond WIDTH

(define (over-width? i)
  (> (+ (invader-x i) (invader-dx i)) WIDTH))


;; Invader -> Number
;; returns the amount an invader's next x is beyond WIDTH by

(define (over-width i)
  (- (+ (invader-x i) (invader-dx i)) WIDTH))



;; Invader -> Boolean
;; returns true if invader's next x is less than 0

(define (less-0? i)
  (< (+ (invader-x i) (invader-dx i)) 0))


;; Invader -> Number
;; returns the amount an invader's next x is less than 0 by

(define (less-0 i)
  (- 0 (+ (invader-x i) (invader-dx i))))



;; Invader Missile -> Boolean
;; returns true if missile is within HIT-RANGE of invader
;; invader has 10 pixel hitbox to bottom
;; within HIT-RANGE of x and HIT-RANGE + 10 of y is hit

(define (hit? i m)
  (and (<= (- (invader-x i) HIT-RANGE) (missile-x m) (+ (invader-x i) HIT-RANGE))
       (<= (- (invader-y i) HIT-RANGE) (missile-y m) (+ (invader-y i) HIT-RANGE))))



;; Invader ListOfMissile -> Boolean
;; returns true if invader is hit by any of the given missiles

(define (hit-missiles? i lom)
  (cond [(empty? lom) false]
        [else
         (if (hit? i (first lom))
             true
             (hit-missiles? i (rest lom)))]))



;; ListOfInvader Missile -> Boolean
;; returns true if missile is hit by any of the given invaders

(define (hit-invaders? loi m)
  (cond [(empty? loi) false]
        [else
         (if (hit? (first loi) m)
             true
             (hit-invaders? (rest loi) m))]))



;; ListOfMissile -> ListOfMissile
;; returns list with topped missiles removed

(define (cull-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (top? (first lom))
             (rest lom)
             (cons (first lom) (cull-missiles (rest lom))))]))


;; Missile -> Boolean
;; returns true if missile is past the top of the scene

(define (top? m)
  (< (missile-y m) 0))


;; ListOfMissile -> ListOfMissile
;; moves the missiles in the list

(define (adv-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (cons (move-missile (first lom))
               (adv-missiles (rest lom)))]))

;; Missile -> Missile
;; move missile-y according to its speed

(define (move-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))



;; Tank -> Tank
;; move tank-x according to its dx, return same tank if at edge

(define (adv-tank t)
  (cond [(and (> (tank-dir t) 0) (< (- WIDTH (tank-x t)) TANK-SPEED)) t]
        [(and (< (tank-dir t) 0) (< (- (tank-x t) 0) TANK-SPEED)) t]
        [else
         (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED))
                    (tank-dir t))]))
        
        


;; Game -> Image
;; draw the game elements on to the scene
#;
(define (draw-game g) BACKGROUND)

(define (draw-game g)
  (draw-invaders (game-invaders g)
                 (draw-missiles (game-missiles g)
                                (draw-tank (game-tank g) BACKGROUND))))


;; ListOfInvader Image -> Image
;; draws each invader onto the scene

(define (draw-invaders loi s)
  (cond [(empty? loi) s]
        [else (place-image INVADER
                           (invader-x (first loi))
                           (invader-y (first loi))
                           (draw-invaders (rest loi) s))]))


;; ListOfMissile Image -> Image
;; draws each missile onto the scene

(define (draw-missiles lom s)
  (cond [(empty? lom) s]
        [else (place-image MISSILE
                           (missile-x (first lom))
                           (missile-y (first lom))
                           (draw-missiles (rest lom) s))]))


;; Tank Image -> Image
;; draws the given tank onto the scene

(define (draw-tank t s)
  (place-image TANK (tank-x t) TANK-Y s))

;; Game KeyEvent -> Game
;; move tank when arrow keys pressed, shoot missile when spacebar pressed
#;
(define (handle-key g k) G0)

(define (handle-key g k)
  (cond [(string=? k "right") (change-tank g "right")]
        [(string=? k "left") (change-tank g "left")]
        [(string=? k " ") (make-game (game-invaders g)
                                     (cons (spawn-missile (game-tank g))
                                           (game-missiles g))
                                     (game-tank g))]
        [else g]))



;; Game String -> Game
;; changes tank dir to 1 if string is "right" and to -1 if it is "left"

(define (change-tank g s)
  (cond [(string=? s "right") (make-game (game-invaders g)
                                         (game-missiles g)
                                         (make-tank (tank-x (game-tank g)) 1))]
        [(string=? s "left") (make-game (game-invaders g)
                                        (game-missiles g)
                                        (make-tank (tank-x (game-tank g)) -1))]))



;; Tank -> Missile
;; makes a new missile at tanks position

(define (spawn-missile t)
  (make-missile (tank-x t) TANK-Y))
          

;; Game -> Boolean
;; determine when game is over, ie. any invader has reached the bottom
(check-expect (game-over? G0) false)
(check-expect (game-over? G2) false)
(check-expect (game-over? G3) true)
(check-expect (game-over? (make-game (list I1 I3 I2) empty T1)) true)
#;
(define (game-over? g) false) ;stub

;; template copied

(define (game-over? g)
  (cond [(empty? (game-invaders g)) false]
        [(bottom? (first (game-invaders g))) true]
        [else
         (game-over? (make-game (rest (game-invaders g)) empty empty))]))


;; Invader -> Boolean
;; returns true if invader-y is >= HEIGHT
;; assume invader is non empty

(check-expect (bottom? I1) false)
(check-expect (bottom? I2) true)
(check-expect (bottom? I3) true)

(define (bottom? i)
  (>= (invader-y i) HEIGHT))






