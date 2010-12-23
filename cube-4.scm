; File: cube-4.scm
; ----------------
; A script for Lacking Sound Festival Listen 41
; performance. This is for workspace 4, which
; generates Tonematrix. A cube is controlled by
; the user and a tone can be placed underneath it.
;
; Key Bindings
; ------------
; 1: start horizontal sequence
; 2: start vertical sequence
; h: roll a cube to left
; j: roll a cube to front
; k: roll a cube to back
; l: roll a cube to right
; SPACE: toggle a tone
; 
; Author: Akinori Kinoshita
; E-mail: art.akinoshi -at- gmail.com
; Date: Sun Nov 21 01:11:23 CST 2010

(clear)

(translate (vector 0 -4 -0.5))

(define l (make-light 'point 'free))
(light-diffuse l (vector 1 1 1))
(light-position l (vector 0 2.5 0))
(shadow-light l)

(hint-cast-shadow)

(define size 6)
(define g-size 9) ; must be an odd number (max: 17)
(define i size)
(define pi (* 4.0 (atan 1.0)))
(define step (/ (/ pi 2) size))
(define (rad->deg rad) (* rad (/ 180 pi)))
(define key-debounce #f)
(define rotated? #t)
(define moving? #f)
(define direction "")
(define pos-x (/ (- g-size 1) 2))
(define pos-y (/ (- g-size 1) 2))
(define c (vector 1 1 1))
(define metro 500)
(define last-time (* 1000 (time)))
(define seq-cnt 0)
(define horiz? #f)
(define vert? #f)
(define surface? #f)
(define tron? #f)
(define horiz-cnt 0)
(define horiz-metro 500)
(define last-horiz-time (* 1000 (time)))
(define vert-cnt 0)
(define vert-metro 500)
(define last-vert-time (* 1000 (time)))
(define note-list '())
(define note2-list '())

(osc-destination "osc.udp://192.168.0.100:12000")
(osc-source "12001")

; create a ristricted cubic space
(with-state
    (hint-none)
    (hint-wire)
    (backfacecull 0)
    (translate (vector 0 (/ (- g-size 1) 2) 0))
    (scale g-size)
    (build-cube))

(define ground
    (with-state
        ;(hint-wire)
        (wire-colour (vector 0 0 0))
        (hint-vertcols)
        (translate (vector 0 -0.5 0))
        (rotate (vector -90 0 0))
        (scale g-size)
        (build-seg-plane g-size g-size)))

(with-primitive ground
    (pdata-map! (lambda (c) (vector .4 .4 .4)) "c"))

(define p (build-particles (* g-size g-size)))

(with-primitive p
    (pdata-index-map! (lambda (i p)
        (vector (- (quotient i g-size) (/ (- g-size 1) 2))
                   1
                   (- (/ (- g-size 1) 2) (modulo i g-size)))) "p")
    (pdata-map! (lambda (c) (vector 1 0 0)) "c"))

(define (update-particles)
    (with-primitive p
        (pdata-op "+" "p" (vector 0 0.005 0))
        (pdata-index-map! (lambda (i x)
            (if (> (vy x) (- (/ g-size 2) 0.5))
                (vector (vx x) (- (/ g-size 2) 0.5) (vz x))
                x))
            "p")))

(define surface
    (with-state
        ;(hint-wire)
        ;(wire-colour (vector 0 0 0))
        (colour (vector 0 0 1))
        (texture (load-texture "gradient.png"))
        (translate (vector 0 (- g-size 0.5) 0))
        (rotate (vector 90 0 0))
        (scale g-size)
        (build-nurbs-plane g-size g-size)))

(define (render)
    (with-primitive surface
        (pdata-index-map!
            (lambda (i p)
                (if (= 100 i)
                    (vector (vx p) (vy p) 1) ; max: 2.5
                    p))
            "p")))
            
;(render)

(define (update-surface)
    (with-primitive surface
        (pdata-op "+" "p" (vector 0 0 -0.01))
        (pdata-index-map! (lambda (i p)
            (if (<= (vz p) 0)
                (vector (vx p) (vy p) 0)
                p))
        "p")))

#;(with-primitive surface
    (pdata-index-map! (lambda (i p)
        (vector (- (/ (- g-size 1) 2) (modulo i g-size))
                (- g-size 0.5)
                (- (quotient i g-size) (/ (- g-size 1) 2)))) "p"))

(define obj
    (with-state
        ;(hint-origin)
        (hint-vertcols)
        (build-cube)))

(with-primitive obj
    (pdata-map! (lambda (c) (vector 0.99 0.73 0.18)) "c"))

(define x (- (* (/ 1 (sqrt 2)) (cos (+ step (/ pi 4)))) 0.5))
(define y (- (* (/ 1 (sqrt 2)) (sin (+ step (/ pi 4)))) 0.5))

(define (move)
    (with-primitive obj
        (translate (vector x y 0))
        (rotate (vector 0 0 (rad->deg step)))))

; key listener
(define (check-key) (cond
    ((key-pressed "1")
     (when (not key-debounce)
     (set! key-debounce #t)
     (set! horiz-cnt 0)
     (set! horiz? (not horiz?))))
    ((key-pressed "2")
     (when (not key-debounce)
     (set! key-debounce #t)
     (set! vert-cnt 0)
     (set! vert? (not vert?))))
    ((key-pressed "h")
     (when (and (not key-debounce) (not moving?) (not (= pos-y 0)))
     (set! key-debounce #t)
     (set! rotated? #f)
     (set! moving? #t)
     (set! direction "h")
     (set! i 0)))
    ((key-pressed "j")
     (when (and (not key-debounce) (not moving?) (not (= pos-x 0)))
     (set! key-debounce #t)
     (set! rotated? #f)
     (set! moving? #t)
     (set! direction "j")
     (set! i 0)
     (with-primitive obj
        (rotate (vector 0 90 0)))))
    ((key-pressed "k")
     (when (and (not key-debounce) (not moving?) (not (= pos-x (- g-size 1))))
     (set! key-debounce #t)
     (set! rotated? #f)
     (set! moving? #t)
     (set! direction "k")
     (set! i 0)
     (with-primitive obj
        (rotate (vector 0 -90 0)))))
    ((key-pressed "l")
     (when (and (not key-debounce) (not moving?) (not (= pos-y (- g-size 1))))
     (set! key-debounce #t)
     (set! rotated? #f)
     (set! moving? #t)
     (set! direction "l")
     (set! i 0)
     (with-primitive obj
        (rotate (vector 0 180 0)))))
    ((key-pressed " ")
     (when (not key-debounce)
     (set! key-debounce #t)
     (make-note)))
    (else (set! key-debounce #f))))

(define (update)
    (if (>= i size)
        (when (not rotated?)
            (set! rotated? #t)
            (with-primitive obj (cond
                ((string=? direction "h")
                 (rotate (vector 0 0 -90))
                 (set! pos-y (- pos-y 1)))
                ((string=? direction "j")
                 (rotate (vector -90 0 -90))
                 (set! pos-x (- pos-x 1)))
                ((string=? direction "k")
                 (rotate (vector 0 90 0))
                 (rotate (vector 90 0 0))
                 (set! pos-x (+ pos-x 1)))
                ((string=? direction "l")
                 (rotate (vector 0 0 -90))
                 (rotate (vector 0 180 0))
                 (set! pos-y (+ pos-y 1)))))
            ;(make-note)
            (set! moving? #f)
            (osc-send "/click" "f" (list 1)))
        (begin
            (set! i (+ i 1))
            (move))))

(define (make-note)
    (with-primitive ground
        (let ((index (* (+ pos-x (* pos-y g-size)) 4)))
            (if (= (vx (pdata-ref "c" index)) 1)
                (set! c #(0.4 1))
                (set! c #(1 1)))
            (pdata-set! "c" index c)
            (pdata-set! "c" (+ index 1) c)
            (pdata-set! "c" (+ index 2) c)
            (pdata-set! "c" (+ index 3) c))))

(define (check-metro)
    ; horizontal sequence
    (when horiz?
        ; draw sequence line
        (with-state
            (hint-none)
            (hint-wire)
            (wire-colour (vector 1 0 0))
            (translate (vector (- horiz-cnt (/ (- g-size 1) 2)) -0.5 0))
            (scale (vector 1 0 g-size))
            (draw-cube))
        ; draw note cube
        (with-primitive ground
            (set! note-list '())
            (let ((index (* horiz-cnt g-size)))
            (for ((j (in-range g-size)))
                (if (= (vx (pdata-ref "c" (* 4 (+ index j)))) 1)
                    (begin
                    ; draw note cube
                    (with-state
                        (translate (vector (- horiz-cnt (/ (- g-size 1) 2))
                                           0
                                           (- (/ (- g-size 1) 2) j)))
                        (colour (vector 1 0 0 0.5))
                        ; ===== put somthing here for osc =====
                        ;(osc-send "/note" "f" (list j))
                        (draw-cube))
                    ; pull a particle
                    (with-primitive p
                        (pdata-set! "p" (+ index j)
                            (vector (- (quotient (+ index j) g-size)
                                       (/ (- g-size 1) 2))
                                    (- (vy (pdata-ref "p" (+ index j)))
                                       (* 0.005 g-size))
                                    (- (/ (- g-size 1) 2) j))))
                    (set! note-list (cons 1 note-list)))
                    (set! note-list (cons 0 note-list))))))
        (when (member 1 note-list)
              (osc-send "/note" "fffffffff" note-list))
        ; update sequence count
        (when (>= (- (* 1000 (time)) last-horiz-time)
                  horiz-metro)
            (set! last-horiz-time (* 1000 (time)))
            (set! horiz-cnt (+ horiz-cnt 1)))
            (when (>= horiz-cnt g-size)
                (set! horiz-cnt 0)))

    ; vertical sequence
    (when vert?
        ; draw sequence line
        (with-state
            (hint-none)
            (hint-wire)
            (wire-colour (vector 0 0 1))
            (translate (vector 0 -0.5 (- vert-cnt (/ (- g-size 1) 2))))
            (scale (vector g-size 0 1))
            (draw-cube))
        (with-primitive ground
            (set! note2-list '())
            (let ((index (- (- g-size 1) vert-cnt)))
            (for ((j (in-range g-size)))
                (if (= (vx (pdata-ref "c" (* 4 (+ index (* j g-size))))) 1)
                    (begin
                    ; draw note cube
                    (with-state
                        (translate (vector (- j (/ (- g-size 1) 2))
                                           0
                                           (- vert-cnt (/ (- g-size 1) 2))))
                        (colour (vector 0 0 1 0.5))
                        ; ===== put somthing here for osc =====
                        (draw-cube))
                    ; pull surface
                    (with-primitive surface
                        (let ((position (+ (- (- g-size 1) index)
                                           (* j (+ g-size 1)))))
                        (pdata-set! "p" position
                            (vector (vx (pdata-ref "p" position))
                                    (vy (pdata-ref "p" position))
                                    (+ (vz (pdata-ref "p" position))
                                       .1)))))
                    (set! note2-list (cons 1 note2-list)))
                    (set! note2-list (cons 0 note2-list))))))
        (when (member 1 note2-list)
              (osc-send "/note2" "fffffffff" (reverse note2-list)))
        ; update sequence count
        (when (>= (- (* 1000 (time)) last-vert-time)
                  vert-metro)
            (set! last-vert-time (* 1000 (time)))
            (set! vert-cnt (+ vert-cnt 1)))
            (when (>= vert-cnt g-size)
                (set! vert-cnt 0))))

(every-frame
    (begin
        (check-key)
        (check-metro)
        (update-particles)
        ;(render)
        (update-surface)
        (update)))
