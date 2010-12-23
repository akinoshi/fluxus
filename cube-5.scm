; File: cube-5.scm
; ----------------
; A script for Lacking Sound Festival Listen 41
; performance. This is for workspace 5, which
; generates a rubik's cube and note cubes surrounding it.
;
; Key Bindings
; ------------
; 1: up-twist cube 
; 2: bottom-twist cube
; 3: front-twist cube
; 4: back-twist cube
; 5: left-twist cube
; 6: right-twist cube
; SPACE: clear all midi notes
; q: toggle harmonic movement
; w: toggle sine wave movement
; e: toggle rotate movement
; r: toggle tangent movement
; a: (a)dd a note
; 
; Author: Akinori Kinoshita
; E-mail: art.akinoshi -at- gmail.com
; Date: Sun Nov 21 01:18:45 CST 2010

(clear)

(define matrix #(
; blue
#(1 1 1.5) #(1 0 1.5) #(1 -1 1.5)
#(0 1 1.5) #(0 0 1.5) #(0 -1 1.5)
#(-1 1 1.5) #(-1 0 1.5) #(-1 -1 1.5)
; green
#(1 1 -1.5) #(1 0 -1.5) #(1 -1 -1.5)
#(0 1 -1.5) #(0 0 -1.5) #(0 -1 -1.5)
#(-1 1 -1.5) #(-1 0 -1.5) #(-1 -1 -1.5)
; red
#(1.5 1 1) #(1.5 0 1) #(1.5 -1 1)
#(1.5 1 0) #(1.5 0 0) #(1.5 -1 0)
#(1.5 1 -1) #(1.5 0 -1) #(1.5 -1 -1)
; orange
#(-1.5 1 1) #(-1.5 0 1) #(-1.5 -1 1)
#(-1.5 1 0) #(-1.5 0 0) #(-1.5 -1 0)
#(-1.5 1 -1) #(-1.5 0 -1) #(-1.5 -1 -1)
; yellow
#(1 1.5 1) #(1 1.5 0) #(1 1.5 -1)
#(0 1.5 1) #(0 1.5 0) #(0 1.5 -1)
#(-1 1.5 1) #(-1 1.5 0) #(-1 1.5 -1)
; white
#(1 -1.5 1) #(1 -1.5 0) #(1 -1.5 -1)
#(0 -1.5 1) #(0 -1.5 0) #(0 -1.5 -1)
#(-1 -1.5 1) #(-1 -1.5 0) #(-1 -1.5 -1)
))

(define r-matrix #(
; blue
#(0 0 0) #(0 0 0) #(0 0 0)
#(0 0 0) #(0 0 0) #(0 0 0)
#(0 0 0) #(0 0 0) #(0 0 0)
; green
#(0 0 0) #(0 0 0) #(0 0 0)
#(0 0 0) #(0 0 0) #(0 0 0)
#(0 0 0) #(0 0 0) #(0 0 0)
; red
#(0 90 0) #(0 90 0) #(0 90 0)
#(0 90 0) #(0 90 0) #(0 90 0)
#(0 90 0) #(0 90 0) #(0 90 0)
; orange
#(0 90 0) #(0 90 0) #(0 90 0)
#(0 90 0) #(0 90 0) #(0 90 0)
#(0 90 0) #(0 90 0) #(0 90 0)
; yellow
#(90 0 0) #(90 0 0) #(90 0 0)
#(90 0 0) #(90 0 0) #(90 0 0)
#(90 0 0) #(90 0 0) #(90 0 0)
; white
#(90 0 0) #(90 0 0) #(90 0 0)
#(90 0 0) #(90 0 0) #(90 0 0)
#(90 0 0) #(90 0 0) #(90 0 0)
))

(define k-matrix '(
; when vz = 1.5
#(-1 1 2) #(0 1 2) #(1 1 2)
#(-1 0 2) #(0 0 2) #(1 0 2)
#(-1 -1 2) #(0 -1 2) #(1 -1 2)
; when vz = -1.5
#(1 1 -2) #(0 1 -2) #(-1 1 -2)
#(1 0 -2) #(0 0 -2) #(-1 0 -2)
#(1 -1 -2) #(0 -1 -2) #(-1 -1 -2)
; when vx = 1.5
#(2 1 1) #(2 1 0) #(2 1 -1)
#(2 0 1) #(2 0 0) #(2 0 -1)
#(2 -1 1) #(2 -1 0) #(2 -1 -1)
; when vx = -1.5
#(-2 1 -1) #(-2 1 0) #(-2 1 1)
#(-2 0 -1) #(-2 0 0) #(-2 0 1)
#(-2 -1 -1) #(-2 -1 0) #(-2 -1 1)
; when vy = 1.5
#(-1 2 -1) #(0 2 -1) #(1 2 -1)
#(-1 2 0) #(0 2 0) #(1 2 0)
#(-1 2 1) #(0 2 1) #(1 2 1)
; when vy = -1.5
#(1 -2 -1) #(0 -2 -1) #(-1 -2 -1)
#(1 -2 1) #(0 -2 1) #(-1 -2 1)
#(1 -2 0) #(0 -2 0) #(-1 -2 0)
; rest
#(-2 -2 -2) #(-2 -1 -2) #(-2 0 -2) #(-2 1 -2) #(-2 2 -2)
#(-2 -2 -1) #(-2 2 -1) #(-2 -2 0) #(-2 2 0) #(-2 -2 1)
#(-2 2 1) #(-2 -2 2) #(-2 -1 2) #(-2 0 2) #(-2 1 2)
#(-2 2 2) #(-1 -2 2) #(-1 2 2) #(0 -2 2) #(0 2 2)
#(1 -2 2) #(1 2 2) #(2 -2 2) #(2 -1 2) #(2 0 2)
#(2 1 2) #(2 2 2) #(2 -2 1) #(2 2 1) #(2 -2 0)
#(2 2 0) #(2 -2 -1) #(2 2 -1) #(2 -2 -2) #(2 -1 -2)
#(2 0 -2) #(2 1 -2) #(2 2 -2) #(1 -2 -2) #(1 2 -2)
#(0 -2 -2) #(0 2 -2) #(-1 -2 -2) #(-1 2 -2)
)) 

(define pi (* 4.0 (atan 1.0)))
(define (deg->rad deg) (* deg (/ pi 180)))
(define deg 10)
(define clock 0)
(define i 0)
(define axis 0)
(define key-debounce #f)
(define face-turn 0)
(define q? #f)
(define w? #f)
(define e? #f)
(define r? #f)
(define t? #f)
(define note-list '())
(define pos-list '())
(define temp-v 0)
(define j 0)

(osc-destination "osc.udp://192.168.0.100:12000") ; for sending
(osc-source "12001") ; for receiving

(define c (build-list (length k-matrix) (lambda (x) (rndvec))))

(define (draw-key lst ls1 k)
    (when (not (null? lst))
        (with-state
            (hint-none)
            (hint-wire)
            (backfacecull 0)
            (wire-colour (vector 1 .2))
            ;(wire-colour (vector 1 1 1 .2))
            ;(wire-colour (vector (vx (car ls1)) (vy (car ls1)) (vz (car ls1)) .5))
            (line-width 2)
            (translate (car lst))
            ; 1st movement explosion
            (when q?
            (translate (vector (* (gh 0) .5 (vx (car lst)))
                               (* (gh 1) .5 (vy (car lst)))
                               (* (gh 2) .5 (vz (car lst))))))
            ; 2nd movement wave
            (when w?
            (translate (vector 0
                               (sin (+ (vx (car lst)) (time)))
                               0)))
            ; 3rd movement rotation
            (when e?
            (translate (vector (* 5 (sin (+ k (time))))
                               0
                               (* 5 (cos (+ k (time)))))))
            ; 4th movement tangent
            (when r?
            (translate (vector (* (gh 3) (tan (+ k (time))))
                               (* (gh 4) (cos (+ k (time))))
                               (* (gh 5) (sin (+ k (time)))))))
            (draw-cube))
        (when (and (not (null? note-list)) (member (car lst) pos-list))
        (with-state
            (translate (car lst))
            ; get the corresponding matrix vector
            (cond
            ((= (vz (car lst)) 2)
             (set! temp-v (vector (vx (car lst)) (vy (car lst)) 1.5)))
            ((= (vz (car lst)) -2)
             (set! temp-v (vector (vx (car lst)) (vy (car lst)) -1.5)))
            ((= (vx (car lst)) 2)
             (set! temp-v (vector 1.5 (vy (car lst)) (vz (car lst)))))
            ((= (vx (car lst)) -2)
             (set! temp-v (vector -1.5 (vy (car lst)) (vz (car lst)))))
            ((= (vy (car lst)) 2)
             (set! temp-v (vector (vx (car lst)) 1.5 (vz (car lst)))))
            ((= (vy (car lst)) -2)
             (set! temp-v (vector (vx (car lst)) -1.5 (vz (car lst)))))
            )
            ; set colour
            (when (vector? temp-v)
            (if (zero? face-turn)
            (begin
            (clean)
            (set! j
            (- 54 (length (member temp-v (vector->list matrix))))
            ))
            (set! j 99)))
            (cond
            (     (<  j  9)           (colour (vector 0 0 1)))
            ((and (>= j  8) (< j 18)) (colour (vector 0 1 0)))
            ((and (>= j 18) (< j 27)) (colour (vector 1 0 0)))
            ((and (>= j 27) (< j 36)) (colour (vector 1 0.5 0)))
            ((and (>= j 36) (< j 45)) (colour (vector 1 1 0)))
            ( else                    (colour (vector 1 1 1))))
            ; 1st movement explosion
            (when q?
            (translate (vector (* (gh 0) .5 (vx (car lst)))
                               (* (gh 1) .5 (vy (car lst)))
                               (* (gh 2) .5 (vz (car lst))))))
            ; 2nd movement wave
            (when w?
            (translate (vector 0
                               (sin (+ (vx (car lst)) (time)))
                               0)))
            ; 3rd movement rotation
            (when e?
            (translate (vector (* 5 (sin (+ k (time))))
                               0
                               (* 5 (cos (+ k (time)))))))
            ; 4th movement tangent
            (when r?
            (translate (vector (* (gh 3) (tan (+ k (time))))
                               (* (gh 4) (cos (+ k (time))))
                               (* (gh 5) (sin (+ k (time)))))))
            (scale 0.2)
            (hint-wire)
            (wire-colour (rndvec))
            (when (member (car lst) pos-list) (draw-cube))))
        (draw-key (cdr lst) (cdr ls1) (+ k 1))))

;(draw-key k-matrix)

(define (draw) (begin (set! i 0)
    (for-each (lambda (p r)
        (with-state
            ;(hint-unlit)
            (hint-wire)
            ;(hint-ignore-depth)
            (wire-colour (vector 0 0 0))
            (line-width 5)
            (translate p)
            (cond
            (     (< i   9)           (colour (vector 0 0 1)))
            ((and (>= i  8) (< i 18)) (colour (vector 0 1 0)))
            ((and (>= i 18) (< i 27)) (colour (vector 1 0 0)))
            ((and (>= i 27) (< i 36)) (colour (vector 1 0.5 0)))
            ((and (>= i 36) (< i 45)) (colour (vector 1 1 0)))
            ( else                    (colour (vector 1 1 1))))
            (rotate r)
            ;(rotate (vector (* deg clock) 0 0))
            ;(scale 0.5)
            (draw-plane))
            (set! i (+ i 1)))
            (vector->list matrix)
            (vector->list r-matrix))))

#;(define (single-draw x)
    (with-state
        (translate (vector-ref matrix x))
        (rotate (vector-ref r-matrix x))
        (draw-plane)))

; v = vector
; a = axis
; d = direction
(define (transform v a d)
    (cond
    ((= a 1)
    (let ((temp (vy v)))
        (vector-set! v 1 (- (* (vy v) (cos (deg->rad (* d deg))))
                            (* (vz v) (sin (deg->rad (* d deg))))))
        (vector-set! v 2 (+ (* temp (sin (deg->rad (* d deg))))
                            (* (vz v) (cos (deg->rad (* d deg))))))))
    ((= a 2)
    (let ((temp (vx v)))
        (vector-set! v 0 (- (* (vx v) (cos (deg->rad (* d deg))))
                            (* (vz v) (sin (deg->rad (* d deg))))))
        (vector-set! v 2 (+ (* temp (sin (deg->rad (* d deg))))
                            (* (vz v) (cos (deg->rad (* d deg))))))))
    ((= a 3)
    (let ((temp (vx v)))
        (vector-set! v 0 (- (* (vx v) (cos (deg->rad (* d deg))))
                            (* (vy v) (sin (deg->rad (* d deg))))))
        (vector-set! v 1 (+ (* temp (sin (deg->rad (* d deg))))
                            (* (vy v) (cos (deg->rad (* d deg))))))))))

(define (transform-all)
    (transform-all-rec (vector->list matrix) 0))

(define (transform-all-rec lst i)
    (when (not (null? lst))
        (transform (vector-ref matrix i))
        (transform-all-rec (cdr lst) (+ i 1))))

(define (up-twist)
    (up-twist-rec (vector->list matrix)
                  (vector->list r-matrix)
                  0))

(define (up-twist-rec ls0 ls1 i)
    (when (not (null? ls0))
        (when (>= (vy (car ls0)) 1)
            (transform (car ls0) 2 1)
            (if (= (vy (car ls0)) 1.5)
                (vector-set! (car ls1) 2 (+ (vz (car ls1)) deg))
                (vector-set! (car ls1) 1 (- (vy (car ls1)) deg))))
        (up-twist-rec (cdr ls0) (cdr ls1) (+ i 1))))

(define (bottom-twist)
    (bottom-twist-rec (vector->list matrix)
                      (vector->list r-matrix)
                      0))

(define (bottom-twist-rec ls0 ls1 i)
    (when (not (null? ls0))
        (when (<= (vy (car ls0)) -1)
            (transform (car ls0) 2 -1)
            (if (= (vy (car ls0)) -1.5)
                (vector-set! (car ls1) 2 (- (vz (car ls1)) deg))
                (vector-set! (car ls1) 1 (+ (vy (car ls1)) deg))))
        (bottom-twist-rec (cdr ls0) (cdr ls1) (+ i 1))))

(define (front-twist)
    (front-twist-rec (vector->list matrix)
                     (vector->list r-matrix)
                     0))

(define (front-twist-rec ls0 ls1 i)
    (when (not (null? ls0))
        (when (>= (vz (car ls0)) 1)
            (transform (car ls0) 3 1)
            (if (= (vz (car ls0)) 1.5)
                (vector-set! (car ls1) 2 (+ (vz (car ls1)) deg))
                (begin
                (vector-set! (car ls1) 1 (+ (vy (car ls1)) deg))
                (vector-set! (car ls1) 0 90))))
        (front-twist-rec (cdr ls0) (cdr ls1) (+ i 1))))

(define (back-twist)
    (back-twist-rec (vector->list matrix)
                    (vector->list r-matrix)
                    0))

(define (back-twist-rec ls0 ls1 i)
    (when (not (null? ls0))
        (when (<= (vz (car ls0)) -1)
            (transform (car ls0) 3 -1)
            (if (= (vz (car ls0)) -1.5)
                (vector-set! (car ls1) 2 (- (vz (car ls1)) deg))
                (begin
                (vector-set! (car ls1) 1 (- (vy (car ls1)) deg))
                (vector-set! (car ls1) 0 90))))
        (back-twist-rec (cdr ls0) (cdr ls1) (+ i 1))))

(define (left-twist)
    (left-twist-rec (vector->list matrix)
                    (vector->list r-matrix)
                    0))

(define (left-twist-rec ls0 ls1 i)
    (when (not (null? ls0))
        (when (<= (vx (car ls0)) -1)
            (transform (car ls0) 1 -1)
            (vector-set! (car ls1) 0 (- (vx (car ls1)) deg)))
        (left-twist-rec (cdr ls0) (cdr ls1) (+ i 1))))

(define (right-twist)
    (right-twist-rec (vector->list matrix)
                     (vector->list r-matrix)
                     0))

(define (right-twist-rec ls0 ls1 i)
    (when (not (null? ls0))
        (when (>= (vx (car ls0)) 1)
            (transform (car ls0) 1 1)
            (vector-set! (car ls1) 0 (+ (vx (car ls1)) deg)))
        (right-twist-rec (cdr ls0) (cdr ls1) (+ i 1))))

;(transform (vector-ref matrix 0))

(define (clean)
    (clean-matrix)
    (clean-rec (vector->list matrix)
               (vector->list r-matrix)
               0))

(define (clean-rec ls0 ls1 i)
    (when (not (null? ls0))
        (cond
        ((= (abs (vz (car ls0))) 1.5)
         (vector-set! (car ls1) 0 0)
         (vector-set! (car ls1) 1 0)
         (vector-set! (car ls1) 2 0))
        ((= (abs (vx (car ls0))) 1.5)
         (vector-set! (car ls1) 0 0)
         (vector-set! (car ls1) 1 90)
         (vector-set! (car ls1) 2 0))
        ((= (abs (vy (car ls0))) 1.5)
         (vector-set! (car ls1) 0 90)
         (vector-set! (car ls1) 1 0)
         (vector-set! (car ls1) 2 0)))
        (clean-rec (cdr ls0) (cdr ls1) (+ i 1))))

(define (clean-matrix)
    (for ([j (in-range (vector-length matrix))])
        (let ((v (vector-ref matrix j)))
            (vector-set! v 0 (get-closest (vx v)))
            (vector-set! v 1 (get-closest (vy v)))
            (vector-set! v 2 (get-closest (vz v))))))

(define (get-closest x)
    (let ((y (min (abs (- x -1.5))
                  (abs (- x -1))
                  (abs (- x 0))
                  (abs (- x 1))
                  (abs (- x 1.5)))))
    (cond
    ((= y (abs (- x -1.5))) -1.5)
    ((= y (abs (- x -1))) -1)
    ((= y (abs (- x 0))) 0)
    ((= y (abs (- x 1))) 1)
    ((= y (abs (- x 1.5))) 1.5))))

(define (check-key) (cond
    ((key-pressed "1")
     (when (and (not key-debounce) (= face-turn 0))
     (set! key-debounce #t)
     (clean)
     (set! face-turn 1)))
    ((key-pressed "2")
     (when (and (not key-debounce) (= face-turn 0))
     (set! key-debounce #t)
     (clean)
     (set! face-turn 2)))
    ((key-pressed "3")
     (when (and (not key-debounce) (= face-turn 0))
     (set! key-debounce #t)
     (clean)
     (set! face-turn 3)))
    ((key-pressed "4")
     (when (and (not key-debounce) (= face-turn 0))
     (set! key-debounce #t)
     (clean)
     (set! face-turn 4)))
    ((key-pressed "5")
     (when (and (not key-debounce) (= face-turn 0))
     (set! key-debounce #t)
     (clean)
     (set! face-turn 5)))
    ((key-pressed "6")
     (when (and (not key-debounce) (= face-turn 0))
     (set! key-debounce #t)
     (clean)
     (set! face-turn 6)))
    ((key-pressed " ")
     (set! note-list '())
     (set! pos-list '()))
    ((key-pressed "q")
     (when (not key-debounce)
     (set! key-debounce #t)
     (set! q? (not q?))))
    ((key-pressed "w")
     (when (not key-debounce)
     (set! key-debounce #t)
     (set! w? (not w?))))
    ((key-pressed "e")
     (when (not key-debounce)
     (set! key-debounce #t)
     (set! e? (not e?))))
    ((key-pressed "r")
     (when (not key-debounce)
     (set! key-debounce #t)
     (set! r? (not r?))))
    ((key-pressed "t")
     (when (not key-debounce)
     (set! key-debounce #t)
     (set! t? (not t?))))
    ((key-pressed "a")
     (when (not key-debounce)
     (set! key-debounce #t)
     (fill)))
    (else (set! key-debounce #f))))

(define (check-osc)
    (when (and (osc-msg "/fill") t?)
    (let ((note (osc 0)))
    (if (assoc note note-list)
        (let* ((pos (car (cdr (assoc note note-list))))
               (k (- (length k-matrix) (length (member pos k-matrix)))))
        (with-state
            (colour (vector 1 1 1))
            ;(translate (car (cdr (assoc note note-list))))
            (translate pos)
            (when q?
            (translate (vector (* (gh 0) .5 (vx pos))
                               (* (gh 1) .5 (vy pos))
                               (* (gh 2) .5 (vz pos)))))
            (when w?
            (translate (vector 0
                               (sin (+ (vx pos) (time)))
                               0)))
            (when e?
            (translate (vector (* 5 (sin (+ k (time))))
                               0
                               (* 5 (cos (+ k (time)))))))
            (when r?
            (translate (vector (* (gh 3) (tan (+ k (time))))
                               (* (gh 4) (cos (+ k (time))))
                               (* (gh 5) (sin (+ k (time)))))))
            ; get the corresponding matrix vector
            (cond
            ((= (vz pos) 2)
             (set! temp-v (vector (vx pos) (vy pos) 1.5)))
            ((= (vz pos) -2)
             (set! temp-v (vector (vx pos) (vy pos) -1.5)))
            ((= (vx pos) 2)
             (set! temp-v (vector 1.5 (vy pos) (vz pos))))
            ((= (vx pos) -2)
             (set! temp-v (vector -1.5 (vy pos) (vz pos))))
            ((= (vy pos) 2)
             (set! temp-v (vector (vx pos) 1.5 (vz pos))))
            ((= (vy pos) -2)
             (set! temp-v (vector (vx pos) -1.5 (vz pos))))
            )
            ; set colour
            (when (vector? temp-v)
            (if (zero? face-turn)
            (begin
            (clean)
            (set! j
            (- 54 (length (member temp-v (vector->list matrix))))
            ))
            (set! j 99)))
            (cond
            (     (<  j  9)           (colour (vector 0 0 1)))
            ((and (>= j  8) (< j 18)) (colour (vector 0 1 0)))
            ((and (>= j 18) (< j 27)) (colour (vector 1 0 0)))
            ((and (>= j 27) (< j 36)) (colour (vector 1 0.5 0)))
            ((and (>= j 36) (< j 45)) (colour (vector 1 1 0)))
            ( else                    (colour (vector 1 1 1))))
            (draw-cube)))
        (let ((pos (list-ref k-matrix (inexact->exact (round (* (flxrnd) 53))))))
        (set! note-list (cons (list note pos) note-list))
        (set! pos-list (cons pos pos-list)))))))

(define (fill)
    (when t?
    (let ((note (round (* (flxrnd) 10))))
    (if (assoc note note-list)
        (let* ((pos (car (cdr (assoc note note-list))))
               (k (- (length k-matrix) (length (member pos k-matrix)))))
        (with-state
            (colour (vector 1 1 1))
            ;(translate (car (cdr (assoc note note-list))))
            (translate pos)
            (when q?
            (translate (vector (* (gh 0) .5 (vx pos))
                               (* (gh 1) .5 (vy pos))
                               (* (gh 2) .5 (vz pos)))))
            (when w?
            (translate (vector 0
                               (sin (+ (vx pos) (time)))
                               0)))
            (when e?
            (translate (vector (* 5 (sin (+ k (time))))
                               0
                               (* 5 (cos (+ k (time)))))))
            (when r?
            (translate (vector (* (gh 3) (tan (+ k (time))))
                               (* (gh 4) (cos (+ k (time))))
                               (* (gh 5) (sin (+ k (time)))))))
            ; get the corresponding matrix vector
            (cond
            ((= (vz pos) 2)
             (set! temp-v (vector (vx pos) (vy pos) 1.5)))
            ((= (vz pos) -2)
             (set! temp-v (vector (vx pos) (vy pos) -1.5)))
            ((= (vx pos) 2)
             (set! temp-v (vector 1.5 (vy pos) (vz pos))))
            ((= (vx pos) -2)
             (set! temp-v (vector -1.5 (vy pos) (vz pos))))
            ((= (vy pos) 2)
             (set! temp-v (vector (vx pos) 1.5 (vz pos))))
            ((= (vy pos) -2)
             (set! temp-v (vector (vx pos) -1.5 (vz pos))))
            )
            ; set colour
            (when (vector? temp-v)
            (if (zero? face-turn)
            (begin
            (clean)
            (set! j
            (- 54 (length (member temp-v (vector->list matrix))))
            ))
            (set! j 99)))
            (cond
            (     (<  j  9)           (colour (vector 0 0 1)))
            ((and (>= j  8) (< j 18)) (colour (vector 0 1 0)))
            ((and (>= j 18) (< j 27)) (colour (vector 1 0 0)))
            ((and (>= j 27) (< j 36)) (colour (vector 1 0.5 0)))
            ((and (>= j 36) (< j 45)) (colour (vector 1 1 0)))
            ( else                    (colour (vector 1 1 1))))
            (draw-cube)))
        (let ((pos (list-ref k-matrix (inexact->exact (round (* (flxrnd) 53))))))
        (set! note-list (cons (list note pos) note-list))
        (set! pos-list (cons pos pos-list)))))))

(define (single-twist)
    (if (< clock (/ 90 deg))
        (cond
        ((= face-turn 1) (up-twist) (set! clock (+ clock 1)))
        ((= face-turn 2) (bottom-twist) (set! clock (+ clock 1)))
        ((= face-turn 3) (front-twist) (set! clock (+ clock 1)))
        ((= face-turn 4) (back-twist) (set! clock (+ clock 1)))
        ((= face-turn 5) (left-twist) (set! clock (+ clock 1)))
        ((= face-turn 6) (right-twist) (set! clock (+ clock 1))))
        (begin
            (osc-send "/click" "f" (list 1))
            (set! clock 0)
            (set! face-turn 0))))

;(define p (build-particles (length k-matrix)))

#;(with-primitive p
    (pdata-map! (lambda (c) (vector 1 1 1 .2)) "c")
    (pdata-map! (lambda (p) (vmul (grndvec) 5)) "p")
)

(every-frame
    (begin
        (check-key)
        (check-osc)
        (single-twist)
        #;(when (< clock (/ 90 deg)) (begin
            ;(transform (vector-ref matrix 1) 3 1)
            ;(transform-all)
            ;(up-twist)
            ;(bottom-twist)
            ;(front-twist)
            ;(set! clock (+ clock 1))
            ;(single-twist)
            ;(display clock)
            ;(newline)
        ))
        (draw)
        ;(single-draw 0)
        (when t? (draw-key k-matrix c 0))
        #;(with-primitive p
            (rotate (vector 0 1 0)))
))
