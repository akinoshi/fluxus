; File: cube-1.scm
; ----------------
; A script for Lacking Sound Festival Listen 41
; performance. This is for workspace 1, which
; generates some random rolling cubes to form
; a title of "CUBE"
;
; Key Bindings
; ------------
; 1: mode to generate cubes one by one
; 2: mode to generate all the cubes
; 3: mode to generate harmonic moving title of "CUBE"
; 4: mode to generate fixed height title of "CUBE"
; SPACE: roll cube(s)
; a: (a)dd a cube
; l: toggle the harmonic (l)ight
; j: decrement rolling speed
; k: increment rolling speed 
;
; Author: Akinori Kinoshita
; E-mail: art.akinoshi -at- gmail.com
; Date: Sun Nov 21 00:59:10 CST 2010

(require scheme/class)

(clear)

; center position of cubes
(define pos '(
#(-7 0 -2) #(-9 0 -3) #(-11 0 0) #(-9 0 3) #(-7 0 2)
#(-5 0 -0.5) #(-3 - 3) #(-1 0 -0.5)
#(1 0 0) #(3 0 -3) #(5 0 -1.5) #(3 0 0) #(5 0 1.5) #(3 0 3)
#(9 0 -3) #(7 0 0.5) #(9 0 0) #(9.5 0 3)))

; tweening direction (0: horizontal 1: vertical)
(define dir '(
1 0 1 0 1
1 0 1
1 0 1 0 1 0
0 1 0 0))

; size of cubes
(define size '(
1 3 5 3 1
6 3 6
7 3 2 3 2 3
5 6 3 4))

; path to get to the cube position
; h: left, j: down, k: up, l: right
(define path '(
; for C
(h h h h h h h k k)
(h h h h h h h h k k k)
(h h h h h h h h h k k k)
(h h h h h h h h h h k k k)
(h h h h h h h h h h h k k)
(h h h h h h h h h h h k)
(h h h h h h h h h h h)
(h h h h h h h h h h h j)
(h h h h h h h h h h h j j)
(h h h h h h h h h h j j j)
(h h h h h h h h h j j j)
(h h h h h h h h j j j)
(h h h h h h h j j)
; for U
(h h h h h k k k)
(h h h h h k k)
(h h h h h k)
(h h h h h)
(h h h h h j)
(h h h h h j j)
(h h h h j j j)
(h h h j j j)
(h h j j j)
(h k k k)
(h k k)
(h k)
(h)
(h j)
(h j j)
; for B
(l k k k)
(l k k)
(l k)
(l)
(l j)
(l j j)
(l j j j)
(l l k k k)
(l l l k k k)
(l l l l k k k)
(l l l l l k k)
(l l l l l k)
(l l)
(l l l)
(l l l l)
(l l l l l j)
(l l l l l j j)
(l l j j j)
(l l l j j j)
(l l l l j j j)
; for E
(l l l l l l l k k k)
(l l l l l l l l k k k)
(l l l l l l l l l k k k)
(l l l l l l l l l l k k k)
(l l l l l l l l l l l k k k)
(l l l l l l l k k)
(l l l l l l l k)
(l l l l l l l)
(l l l l l l l j)
(l l l l l l l j j)
(l l l l l l l j j j)
(l l l l l l l l j j j)
(l l l l l l l l l j j j)
(l l l l l l l l l l j j j)
(l l l l l l l l l l l j j j)
(l l l l l l l l)
(l l l l l l l l l)
(l l l l l l l l l l)
))

; make wondering path
(set! path (map (lambda (x) (append x '(h j k l
    h j k l h j k l))) path))

(define obj (build-locator))
(define height 5)
(define count 0)
(define key-debounce #f)
(define roll-size 16)
(define i roll-size)
(define pi (* 4.0 (atan 1.0)))
(define (rad->deg rad) (* rad (/ 180 pi)))
(define j 0)
(define step (/ (/ pi 2) roll-size))
(define x (- (* (/ 1 (sqrt 2)) (cos (+ step (/ pi 4)))) 0.5))
(define y (- (* (/ 1 (sqrt 2)) (sin (+ step (/ pi 4)))) 0.5))
(define mode 1)
(define destroy-list '())
(define light? #f)
(define harmonic? #f)
(define step-list '(1 2 4 8 16 32 64))
(define step-index 4)

(osc-destination "osc.udp://192.168.0.100:12000") ; for sending
(osc-source "12001") ; for receiving

(set-camera-transform (mtranslate (vector 0 0 -10)))

(light-diffuse 0 (vector 0 0 0))
(define l (make-light 'point 'free))
(light-diffuse l (vector 1 1 1))
(light-position l (vector 0 25 0))
(shadow-light l)
(shadow-debug 0)

(hint-cast-shadow)

(lock-camera obj)

(with-primitive obj
    (translate (vector -4.5 7 4))
    (rotate (vector 0 -35 0))
    (rotate (vector -55 0 0)))

(with-state
    (translate (vector 0 -0.5 0))
    (rotate (vector 90 0 0))
    (scale 120)
    (build-plane))

(define (build-all ls0 ls1 ls2)
    (when (not (null? ls0))
        (with-state
            (let* ((p (car ls0))
                   (pp (vector (vx p) (* (- height 1) 0.5) (vz p))))
                (translate pp))
            (if (= (car ls1) 0)
                (begin
                    (scale (vector (car ls2) height 1))
                    (rotate (vector 0 90 0)))
                (scale (vector 1 height (car ls2))))
            (set! destroy-list (cons (build-cube) destroy-list)))
        (build-all (cdr ls0) (cdr ls1) (cdr ls2))))

;(build-all pos dir size)

(define (bang-light-position)
    (light-position l (vector 0 (* 10 (gh 0)) 0))
    (light-diffuse l (vector (gh 1) 0 0))
)

(define (reset-light)
    (light-diffuse l (vector 1 1 1))
    (light-position l (vector 0 25 0)))

(define (destroy-all)
    (destroy-all-rec destroy-list))

(define (destroy-all-rec lst)
    (when (not (null? lst))
        (destroy (car lst))
        (destroy-all-rec (cdr lst))))

; key listener
(define (check-key) (cond
    ((key-pressed "1")
     (when (not key-debounce)
     (set! key-debounce #t)
     (set! mode 1)
     (set! j 0)
     (destroy-all)
     (set! destroy-list '())
     (set! cubes '())
     (set! harmonic? #f)))
    ((key-pressed "2")
     (when (not key-debounce)
     (set! key-debounce #t)
     (set! mode 2)
     (destroy-all)
     (set! destroy-list '())
     (set! cubes (build-list (length path)
                 (lambda (x) (make-object cube% x))))
     (set! harmonic? #f)))
    ((key-pressed "3")
     (when (not key-debounce)
     (set! key-debounce #t)
     (set! mode 3)
     (set! j 0)
     (destroy-all)
     (set! destroy-list '())
     (set! cubes '())
     (set! harmonic? #t)))
    ((key-pressed "4")
     (when (not key-debounce)
     (set! key-debounce #t)
     (set! mode 4)
     (set! j 0)
     (destroy-all)
     (set! destroy-list '())
     (set! cubes '())
     (set! harmonic? #f)
     (build-all pos dir size)))
    ((key-pressed " ")
     (when (not key-debounce)
     (set! key-debounce #t)
     ;(send r update)
     (for-each (lambda (p) (send p update)) cubes)))
    ((key-pressed "a")
     (when (and (not key-debounce) (= mode 1))
     (set! key-debounce #t)
     (when (not (>= j (length path)))
         (set! cubes (cons (make-object cube% j) cubes))
         (set! j (+ j 1)))))
    ((key-pressed "l")
     (when (not key-debounce)
     (set! key-debounce #t)
     (if light?
        (set! light? #f)
        (set! light? #t))
     (reset-light)))
    ((key-pressed "j")
     (when (and (not key-debounce) (null? destroy-list))
     (set! key-debounce #t)
     (set! step-index (- step-index 1))
     (when (<= step-index 0) (set! step-index 0))
     (set! roll-size (list-ref step-list step-index))
     (setup)))
    ((key-pressed "k")
     (when (and (not key-debounce) (null? destroy-list))l
     (set! key-debounce #t)
     (set! step-index (+ step-index 1))
     (when (>= step-index (- (length step-list) 1))
        (set! step-index (- (length step-list) 1)))
     (set! roll-size (list-ref step-list step-index))
     (setup)))
    (else (set! key-debounce #f))))

(define (setup) (begin
    (set! i roll-size)
    (set! step (/ (/ pi 2) roll-size))
    (set! x (- (* (/ 1 (sqrt 2)) (cos (+ step (/ pi 4)))) 0.5))
    (set! y (- (* (/ 1 (sqrt 2)) (sin (+ step (/ pi 4)))) 0.5))))

(define (draw-harmonic)
    (draw-harmonic-rec pos dir size 0))

(define (draw-harmonic-rec ls0 ls1 ls2 i)
    (when (not (null? ls0)) (let ((h (* height 0.5)))
        (with-state
            ;(hint-none) (hint-wire) (backfacecull 0)
            (let* ((p (car ls0))
                   (pp (vector (vx p) (* h (gh i) 0.5) (vz p))))
                (translate pp))
            (if (= (car ls1) 0)
                (begin
                    (scale (vector (car ls2) (+ (* h (gh i)) 1) 1))
                    (rotate (vector 0 90 0)))
                (scale (vector 1 (+ (* h (gh i)) 1) (car ls2))))
            ;(hint-normal)
            (draw-cube))
        (draw-harmonic-rec (cdr ls0) (cdr ls1) (cdr ls2) (+ i 1)))))

(define cube%
    (class object%
        (init id_)
        (field (id id_))

        (define cnt roll-size)
        (define direction 0)
        (define moving? #f)
        (define rotated? #t)
        (define path-list (list-ref path id))

        (define obj
            (with-state
                ;(hint-origin)
                ;(colour (vector 1 0 0))
                (let ((o (build-cube)))
                    (set! destroy-list (cons o destroy-list))
                    o)))

        (define/public (update)
            (when (and (not (null? path-list)) (not moving?))
            (set! moving? #t)
            (set! rotated? #f)
            (let ((p (list-ref path-list (inexact->exact
                     (floor (* (flxrnd) (length path-list)))))))
                (set! direction p)
                (set! path-list (remove p path-list)))
            ;(set! direction (car (list-ref path id)))
            (cond
                ((eqv? direction 'h) 0)
                ((eqv? direction 'j)
                     (with-primitive obj
                         (rotate (vector 0 90 0))))
                ((eqv? direction 'k)
                     (with-primitive obj
                         (rotate (vector 0 -90 0))))
                ((eqv? direction 'l)
                     (with-primitive obj
                         (rotate (vector 0 180 0)))))
            (set! cnt 0)
            ;(display path-list)
            ;(newline)
            ))
            
        (define/public (draw)
            (if (>= cnt roll-size)
                (when (not rotated?)
                    (set! rotated? #t)
                    (with-primitive obj (cond
                        ((eqv? direction 'h)
                         (rotate (vector 0 0 -90)))
                        ((eqv? direction 'j)
                         (rotate (vector -90 0 -90)))
                        ((eqv? direction 'k)
                         (rotate (vector 0 90 0))
                         (rotate (vector 90 0 0)))
                        ((eqv? direction 'l)
                         (rotate (vector 0 0 -90))
                         (rotate (vector 0 180 0)))))
                    (set! moving? #f)
                    (osc-send "/click" "f" (list 1)))
                (begin
                    (set! cnt (+ cnt 1))
                    (with-primitive obj
                        (translate (vector x y 0))
                        (rotate (vector 0  0 (rad->deg step)))))))

        (super-new)))

#;(define cubes (build-list (length path)
                (lambda (x) (make-object cube% x))))
;(define cubes (list (make-object cube% 0)))
(define cubes '())

(define (check-osc)
    (when (osc-msg "/roll")
        (for-each (lambda (p) (send p update)) cubes)))

(every-frame
    (begin
        (check-key)
        (check-osc)
        ;(draw-pointer)
        (for-each (lambda (p) (send p update)) cubes)
        (when light? (bang-light-position))
        (when harmonic? (draw-harmonic))
        (for-each (lambda (p) (send p draw)) cubes)
        #;(with-state
            ;(colour (vector 0 1 0))
            (translate (car pos))
            (scale (vector 1 (+ 1 (* 5 (gh 0))) 1))
            (draw-cube))
))
