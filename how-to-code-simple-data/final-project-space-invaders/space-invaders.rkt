;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 2)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 2)
(define TANK-SPEED 5)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define INVADE-THRESHOLD 12)

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

(define TANK-HEIGHT (image-height TANK))
(define TANK-HEIGHT/2 (/ TANK-HEIGHT 2))
(define TANK-Y (- HEIGHT TANK-HEIGHT/2))

(define TANK-WIDTH/2 (/ (image-width TANK) 2))
(define TANK-AT-LEFT-EDGE (+ 0 TANK-WIDTH/2))
(define TANK-AT-RIGHT-EDGE (- WIDTH TANK-WIDTH/2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define MISSILE-HEIGHT/2 (/ (image-height MISSILE) 2))

(define ENDING-IMAGE (text "Game over !" 36 "red"))



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


;; ListOfInvader is one of:
;; - empty
;; (cons Invader ListOfInvader)

(define LOI1 empty)
(define LOI2 (cons I1 empty))
(define LOI3 (cons I1 (cons I2 empty)))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else (... (fn-for-invader (first loi))
                   (fn-for-loi (rest loi)))]))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit I1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit I1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit I1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))


;; ListOfMissile is one of:
;; - empty
;; (cons Missile ListOfMissile)

(define LOM1 empty)
(define LOM2 (cons M1 empty))
(define LOM3 (cons M1 (cons M2 empty)))
(define LOM4 (list M2 M3))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else (... (fn-for-missile (first lom))
                   (fn-for-lom (rest lom)))]))

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))



;; =================
;; Functions:

;; Game -> Game
;; start the world with ...
;; 
(define (main g)
  (big-bang g                                   ; Game
    (on-tick   next-game)                       ; Game -> Game
    (to-draw   render-game)                     ; Game -> Image
    (stop-when game-over? draw-ending)         ; Game -> Boolean
    (on-key    handle-key)))                    ; Game KeyEvent -> Game

;; Game -> Game
;; produce the next game state
(check-random (next-game (make-game empty empty (make-tank 50 1)))
              (make-game (next-invaders (add-invader-randomly empty)) empty (next-tank (make-tank 50 1)))) ; tank away from edges, left -> right

(check-random (next-game (make-game empty empty (make-tank 50 -1)))
              (make-game (next-invaders (add-invader-randomly empty)) empty (next-tank (make-tank 50 -1)))) ; tank away from edges, right -> left

(check-random (next-game (make-game empty empty (make-tank (- WIDTH TANK-SPEED) 1)))
              (make-game (next-invaders (add-invader-randomly empty)) empty (next-tank (make-tank (- WIDTH TANK-SPEED) 1))))

(check-random (next-game G2)
              (make-game (next-invaders (add-invader-randomly (list I1))) (next-missiles (list M1)) (next-tank T1)))

(check-random (next-game G3)
              (make-game (next-invaders (add-invader-randomly (list I2))) (next-missiles (list M1)) (next-tank T1)))

;(define (next-game g) g)

(define (next-game g)
  (make-game (next-invaders (add-invader-randomly (filter-collided-invaders (game-invaders g) (filter-off-missiles (game-missiles g)))))
             (next-missiles (filter-collided-missiles (filter-off-missiles (game-missiles g)) (game-invaders g)))
             (next-tank (game-tank g))))

;; Tank -> Tank
;; produce next tank state, change direction if at edges
(check-expect (next-tank (make-tank 50 1)) (make-tank (+ 50 (* 1 TANK-SPEED)) 1))
(check-expect (next-tank (make-tank 50 -1)) (make-tank (+ 50 (* -1 TANK-SPEED)) -1))
(check-expect (next-tank (make-tank (- TANK-AT-RIGHT-EDGE TANK-SPEED) 1)) (make-tank TANK-AT-RIGHT-EDGE 1))
(check-expect (next-tank (make-tank TANK-AT-RIGHT-EDGE -1)) (make-tank (+ TANK-AT-RIGHT-EDGE  (* -1 TANK-SPEED)) -1))
(check-expect (next-tank (make-tank (+ TANK-AT-LEFT-EDGE TANK-SPEED) -1)) (make-tank TANK-AT-LEFT-EDGE -1))
(check-expect (next-tank (make-tank TANK-AT-LEFT-EDGE 1)) (make-tank (+ 0 TANK-WIDTH/2 (* 1 TANK-SPEED)) 1))

;(define (next-tank t) t) ;stub

(define (next-tank t)
  (cond [(>= (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) TANK-AT-RIGHT-EDGE)
         (make-tank TANK-AT-RIGHT-EDGE (tank-dir t))]
        [(<= (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) TANK-AT-LEFT-EDGE)
         (make-tank TANK-AT-LEFT-EDGE (tank-dir t))]
        [else 
         (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (tank-dir t))]))


;; ListOfInvader -> ListOfInvader
;; produce next invaders state, adding invaders randomly
(check-expect (next-invaders empty) empty)
(check-expect (next-invaders (cons (make-invader 100 80 10) empty)) (cons (make-invader (+ 100 (* 10 INVADER-X-SPEED))
                                                                                        (+ 80  (* 10 INVADER-Y-SPEED))
                                                                                        10)
                                                                          empty))
(define (next-invaders loi)
  (cond [(empty? loi) loi]
        [else (cons (next-invader (first loi))
                    (next-invaders (rest loi)))]))

;; Invader -> Invader
;; next invader position by INVADER-X-SPEED AND INVADER-Y-SPEED, reverse dir if at wall
(check-expect (next-invader I1) (make-invader (+ (* (invader-dx I1) INVADER-X-SPEED) (invader-x I1))
                                              (+ (* (abs (invader-dx I1)) INVADER-Y-SPEED) (invader-y I1))
                                              (invader-dx I1)))
  
(check-expect (next-invader (make-invader (- WIDTH INVADER-X-SPEED) 100 1))
              (make-invader   WIDTH (+ (* 1 INVADER-Y-SPEED) 100) 1))

(check-expect (next-invader (make-invader (- WIDTH 1) 100 1))
              (make-invader   WIDTH (+ (* 1 INVADER-Y-SPEED) 100) -1))
  
(check-expect (next-invader (make-invader 1 150 -1))
              (make-invader  0 (+ (* 1 INVADER-Y-SPEED) 150) 1))
;(define (next-invader i) i)

(define (next-invader i)
  (cond [(> (+ (invader-x i) (* INVADER-X-SPEED (invader-dx i))) WIDTH)
         (make-invader WIDTH (+ (* INVADER-Y-SPEED (invader-dx i)) (invader-y i)) (- (invader-dx i)))]
        [(< (+ (invader-x i) (* INVADER-X-SPEED (invader-dx i))) 0)
         (make-invader 0 (+ (* INVADER-Y-SPEED (abs (invader-dx i))) (invader-y i)) (- (invader-dx i)))]
        [else 
         (make-invader (+ (invader-x i)  (* INVADER-X-SPEED (invader-dx i)))
                       (+ (invader-y i)  (* INVADER-Y-SPEED (abs (invader-dx i)))) (invader-dx i))]))

;; ListOfInvader -> ListOfInvader
;; add a new invader randomly based on a threshold
(check-random (add-invader-randomly empty) (if (< (random INVADE-RATE) INVADE-THRESHOLD)
                                               (cons (random-invader) empty)
                                               empty))

;(define (add-invader-randomly loi) loi)

(define (add-invader-randomly loi)
  (if (< (random INVADE-RATE) INVADE-THRESHOLD)
      (cons (random-invader) loi)
      loi))

;; Void -> Invader
;; produce random invader
(check-random (random-invader) (make-invader (random (/ WIDTH 2)) (- (random (/ HEIGHT 2))) 1))

;(define (randmon-invader) I1)

(define (random-invader)
  (make-invader (random (/ WIDTH 2)) (- (random (/ HEIGHT 2))) 1))


;; ListOfInvader ListOfMissile -> ListOfInvader
;; remove Invaders that are hit by missiles
;; ASSUME: missiles are on-screen
(check-expect (filter-collided-invaders empty empty) empty)
(check-expect (filter-collided-invaders (list I1) (list M1)) (list I1))
(check-expect (filter-collided-invaders (list (make-invader 50 100 1) (make-invader 200 200 1)) (list (make-missile 200 190)))
              (list (make-invader 50 100 1)))
              
;(define (filter-collided-invaders loi lom) loi)

(define (filter-collided-invaders loi lom)
  (cond [(empty? loi) loi]
        [else (if (invader-collided? (first loi) lom)
                  (rest loi)
                  (cons (first loi)
                        (filter-collided-invaders (rest loi) lom)))]))


;; Invader ListOfMissile -> Boolean
;; produce true if invader has collided with one of the missiles on-screen, else false
(check-expect (invader-collided? I1 LOM1) false)
(check-expect (invader-collided? I1 LOM3) true)
(check-expect (invader-collided? (make-invader 100 -20 1) (list (make-missile 100 -15))) false)
(check-expect (invader-collided? (make-invader 250 100 1) (list (make-missile 255 103))) true)
(check-expect (invader-collided? (make-invader 100 -33 1) (list (make-missile 102 30))) false)
(check-expect (invader-collided? (make-invader 100 (- MISSILE-HEIGHT/2) 1)
                                 (list (make-missile 102 (- MISSILE-HEIGHT/2))))
              false)



;(define (invader-collided? i lom) false)

(define (invader-collided? i lom)
  (cond [(empty? lom) false]
        [else (if (and (invader-on? i) (missile-hit? i (first lom)))
                  true
                  (invader-collided? i (rest lom)))]))


;; Invader -> Boolean
;; produce true if invader is on-screen, else false
(check-expect (invader-on? (make-invader 50 50 1)) true)
(check-expect (invader-on? (make-invader -50 -50 1)) false)
(check-expect (invader-on? (make-invader 200 0 1)) true)
(check-expect (invader-on? (make-invader -50 -1 1)) false)

;(define (invader-on? i) false)

(define (invader-on? invader)
  (not (negative? (invader-y invader))))

;; Invader Missile -> Boolean
;; produce true if missile in the hit range of the invader, else false
(check-expect (missile-hit? I1 M1) false)
(check-expect (missile-hit? I1 M2) true)
(check-expect (missile-hit? (make-invader 100 -20 1) (make-missile 100 -15)) true)
(check-expect (missile-hit? (make-invader 200 100 1) (make-missile 195 105)) true)
(check-expect (missile-hit? (make-invader 200 (- MISSILE-HEIGHT/2) 1)
                            (make-missile 205 (- MISSILE-HEIGHT/2)))
              true)

;(define (missile-hit? i m) false)

(define (missile-hit? i m)
  (and (<= (abs (- (missile-y m) (invader-y i))) HIT-RANGE)
       (<= (abs (- (missile-x m) (invader-x i))) HIT-RANGE)))


;; ListOfMissile -> ListOfMissile
;; produce next missiles state
(check-expect (next-missiles empty) empty)
(check-expect (next-missiles (cons (make-missile 30 50) empty)) (cons (make-missile 30 (- 50 MISSILE-SPEED)) empty))
(check-expect (next-missiles (cons (make-missile 30 50) (cons (make-missile 40 40) empty)))
              (cons (make-missile 30 (- 50 MISSILE-SPEED)) (cons (make-missile 40 (- 40 MISSILE-SPEED)) empty)))

;(define (next-missiles lom) lom)

(define (next-missiles lom)
  (cond [(empty? lom) empty]
        [else (cons (next-missile (first lom))
                    (next-missiles (rest lom)))]))


;; Missile -> Missile
;; produce next missile state
(check-expect (next-missile (make-missile 50 50)) (make-missile 50 (- 50 MISSILE-SPEED)))

;(define (next-missile M1) M1)

(define (next-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; ListOfMissile ListOfInvader -> ListOfMissile
;; filter missiles that hit invaders
;; ASSUME: missiles are on-screen
(check-expect (filter-collided-missiles empty empty) empty)
(check-expect (filter-collided-missiles LOM2 LOI2) LOM2)
(check-expect (filter-collided-missiles LOM3 LOI2) (list M1))
(check-expect (filter-collided-missiles (list (make-missile 100 -1)) LOI3) (list (make-missile 100 -1)))
(check-expect (filter-collided-missiles (list (make-missile 100 0)) LOI3) (list (make-missile 100 0)))
(check-expect (filter-collided-missiles (list (make-missile 200 10) (make-missile 100 200))
                                        (list (make-invader 100 195 1)))
              (list (make-missile 200 10)))
 
;(define (filter-collided-missiles lom loi) lom)

(define (filter-collided-missiles lom loi)
  (cond [(empty? lom) empty]
        [else (if  (missile-collided? (first lom) loi)
                   (rest lom)
                   (cons (first lom) 
                         (filter-collided-missiles (rest lom) loi)))]))


;; Missile ListOfInvader -> Boolean
;; produce true if missile has collided with one of on-screen the invaders, else false
;; ASSUME: missiles are on-screen
(check-expect (missile-collided? M1 empty) false)
(check-expect (missile-collided? M1 LOI2) false)
(check-expect (missile-collided? M2 LOI3) true)
(check-expect (missile-collided? (make-missile 100 100) (list (make-invader 105 105 1))) true)
(check-expect (missile-collided? (make-missile 100 30) (list (make-invader 105 -35 1))) false)
(check-expect (missile-collided? (make-missile 100 20) (list (make-invader 105 23 1))) true)

;(define (missile-collided? m loi) false)

(define (missile-collided? m loi)
  (cond [(empty? loi) false]
        [else (if (and (invader-on? (first loi)) (missile-hit? (first loi) m))
                  true
                  (missile-collided? m (rest loi)))]))


;; ListOfMissile -> ListOfMissile
;; remove off screen missiles
(check-expect (filter-off-missiles empty) empty)
(check-expect (filter-off-missiles (list (make-missile 100 -10))) empty)
(check-expect (filter-off-missiles (list (make-missile 100 -10) (make-missile 100 100))) (list (make-missile 100 100)))
(check-expect (filter-off-missiles (list (make-missile 100 0) (make-missile 100 100))) (list (make-missile 100 0)
                                                                                             (make-missile 100 100)))

(check-expect (filter-off-missiles (list (make-missile 100 -10) (make-missile 100 100))) (list (make-missile 100 100)))

;(define (filter-off-missiles lom) lom)

(define (filter-off-missiles lom)
  (cond [(empty? lom) empty]
        [else (if (missile-off? (first lom))
                  (rest lom)
                  (cons (first lom) (filter-off-missiles (rest lom))))]))


;; Missile -> Boolean
;; produce true if missile is off screen, else false
(check-expect (missile-off? (make-missile 100 100)) false)
(check-expect (missile-off? (make-missile 200 -40)) true)
(check-expect (missile-off? (make-missile 100 (- MISSILE-HEIGHT/2))) true)
(check-expect (missile-off? (make-missile 111 0)) false)

;(define (missile-off? m) false)

(define (missile-off? m)
  (<= (missile-y m) (- MISSILE-HEIGHT/2 )))

;; Game -> Image
;; render the game 
(check-expect (render-game G0) (place-image TANK (/ WIDTH 2) TANK-Y BACKGROUND))
(check-expect (render-game G2) (place-image TANK 50 TANK-Y
                                            (place-image MISSILE 150 300
                                                         (place-image INVADER 150 100 BACKGROUND))))
;(define (render-game g) empty-image)

(define (render-game g)
  (render-tank (game-tank g)
               (render-invaders (game-invaders g)
                                (render-missiles (game-missiles g) BACKGROUND))))


;; Tank Image -> Image
;; render the tank on the screen on an Image
(check-expect (render-tank (make-tank 60 1) BACKGROUND)
              (place-image TANK 60 TANK-Y BACKGROUND))

(check-expect (render-tank (make-tank 60 1) (render-invaders LOI1 BACKGROUND))
              (place-image TANK 60 TANK-Y (render-invaders LOI1 BACKGROUND)))

(check-expect (render-tank (make-tank 60 1) (render-invaders LOI1 (render-missiles LOM1 BACKGROUND)))
              (place-image TANK 60 TANK-Y (render-invaders LOI1  (render-missiles LOM1 BACKGROUND))))

;(define (render-tank t img) BACKGROUND)

(define (render-tank t img)
  (place-image TANK (tank-x t) TANK-Y img))

;; ListOfInvaders Image -> Image
;; render a list of invaders on an Image
(check-expect (render-invaders LOI1 BACKGROUND) BACKGROUND)
(check-expect (render-invaders LOI2 BACKGROUND) (place-image INVADER 150 100 BACKGROUND))
(check-expect (render-invaders LOI3 (render-missiles LOM2 BACKGROUND)) (place-image INVADER 150 100
                                                                                    (place-image INVADER 150 HEIGHT (render-missiles LOM2 BACKGROUND))))

;(define (render-invaders loi img) BACKGROUND)

(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else (place-image INVADER (invader-x (first loi)) (invader-y (first loi))
                           (render-invaders (rest loi) img))]))

;; ListOfMissile Image -> Image
;; render a list of missiles on an Image
(check-expect (render-missiles LOM1 BACKGROUND) BACKGROUND)
(check-expect (render-missiles LOM2 BACKGROUND) (place-image MISSILE 150 300 BACKGROUND))
(check-expect (render-missiles LOM3 BACKGROUND) (place-image MISSILE 150 300 (place-image MISSILE 150 110 BACKGROUND)))

;(define (render-missiles lom img) BACKGROUND)

(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else (place-image MISSILE (missile-x (first lom)) (missile-y (first lom))

                           (render-missiles (rest lom) img))]))

;; Gamge -> Image
;; render ending text image at the center of the last game scene
(check-expect (draw-ending G0) (place-image ENDING-IMAGE (/ WIDTH 2) (/ HEIGHT 2) (render-game G0)))
(check-expect (draw-ending G1) (place-image ENDING-IMAGE (/ WIDTH 2) (/ HEIGHT 2) (render-game G1)))
(check-expect (draw-ending G2) (place-image ENDING-IMAGE (/ WIDTH 2) (/ HEIGHT 2) (render-game G2)))

(define (draw-ending g)
  (place-image ENDING-IMAGE (/ WIDTH 2) (/ HEIGHT 2) (render-game g)))

;; Game -> Boolean
;; stop the game when on of the invaders reache the bottom of the screen
(check-expect (game-over? G0) false)
(check-expect (game-over? G1) false)
(check-expect (game-over? G2) false)
(check-expect (game-over? (make-game (list I1 I2 I3) LOM3 T1)) true)

;(define (game-over? g) false)

(define (game-over? g)
  (invaders-at-bottom? (game-invaders g)))

;; ListOfInvader -> Boolean
;; produce true if one of the invaders is at the bottom of the screen
(check-expect (invaders-at-bottom? LOI1) false)
(check-expect (invaders-at-bottom? LOI2) false)
(check-expect (invaders-at-bottom? LOI3) true)
(check-expect (invaders-at-bottom? (list I1 I2 I3)) true)

;(define (invaders-at-bottom? loi) false)

(define (invaders-at-bottom? loi)
  (cond [(empty? loi) false]
        [else (or (>= (invader-y (first loi)) HEIGHT)
                  (invaders-at-bottom? (rest loi)))]))

;; Game KeyEvent -> Game
;; handle key event (left), (right) and (spacebar)
(check-expect (handle-key G0 "left") (make-game empty empty (make-tank (tank-x T0) -1)))
(check-expect (handle-key G0 "right") (make-game empty empty (make-tank (tank-x T0) 1)))
(check-expect (handle-key G0 " ") (make-game empty
                                             (cons (make-missile (tank-x T0) (- HEIGHT TANK-HEIGHT)) empty)
                                             (make-tank (tank-x T0) (tank-dir T0))))

(check-expect (handle-key G3 "d") G3)

;(define (handle-key g ke) g)

(define (handle-key g ke)
  (cond [(key=? ke "left")
         (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) -1))]
        [(key=? ke "right")
         (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) 1))]
        [(key=? ke " ")
         (make-game (game-invaders g)
                    (cons (make-missile (tank-x (game-tank g)) (- HEIGHT TANK-HEIGHT)) (game-missiles g))
                    (game-tank g))]
        [else (make-game (game-invaders g) (game-missiles g) (game-tank g))]))
