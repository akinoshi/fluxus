; File: cube-6.scm
; ----------------
; A script for Lacking Sound Festival Listen 41
; performance. This is for workspace 6, which
; generates a compound of five cubes.
;
; Key Bindings
; ------------
; 1: toggle first cube
; 2: toggle second cube
; 3: toggle third cube
; 4: toggle forth cube
; 5: toggle fifth cube
; q: toggle harmonic movement for first cube
; w: toggle harmonic movement for second cube
; e: toggle harmonic movement for third cube
; r: toggle harmonic movement for forth cube
; t: toggle harmonic movement for fifth cube
; 
; Author: Akinori Kinoshita
; E-mail: art.akinoshi -at- gmail.com
; Date: Sun Nov 21 01:26:44 CST 2010

(clear)

;(hint-unlit)

(define A (* 90 (atan (* 2 (- 0.5 (/ (- (sqrt 5) 2) (- 3 (sqrt 5))))))))

(define key-debounce #f)
(define 1? #f)
(define 2? #f)
(define 3? #f)
(define 4? #f)
(define 5? #f)
(define q? #f)
(define w? #f)
(define e? #f)
(define r? #f)
(define t? #f)

(define (render)
(scale (vector 5 5 5))
(push)
  (rotate (vector 0 0 0))
  ;(rotate (vector A 0 0))
  (rotate (vector (* A (sin (time))) 0 0))
  (when q? (rotate (vector (* 30 (gh 0)) 0 0)))
  (colour (vector 1 0 0))
  (when 1? (draw-cube))
(pop)
(push)
  (rotate (vector 0 72 0))
  ;(rotate (vector A 0 0))
  (rotate (vector (* A (sin (time))) 0 0))
  (when w? (rotate (vector (* 30 (gh 1)) 0 0)))
  (colour (vector 1 1 0))
  (when 2? (draw-cube))
(pop)
(push)
  (rotate (vector 0 144 0))
  ;(rotate (vector A 0 0))
  (rotate (vector (* A (sin (time))) 0 0))
  (when e? (rotate (vector (* 30 (gh 2)) 0 0)))
  (colour (vector 0 1 0))
  (when 3? (draw-cube))
(pop)
(push)
  (rotate (vector 0 -144 0))
  ;(rotate (vector A 0 0))
  (rotate (vector (* A (sin (time))) 0 0))
  (when r? (rotate (vector (* 30 (gh 3)) 0 0)))
  (colour (vector 0 1 1))
  (when 4? (draw-cube))
(pop)
(push)
  (rotate (vector 0 -72 0))
  ;(rotate (vector A 0 0))
  (rotate (vector (* A (sin (time))) 0 0))
  (when t? (rotate (vector (* 30 (gh 4)) 0 0)))
  (colour (vector 0 0 1))
  (when 5? (draw-cube))
(pop))

(define (check-key) (cond
    ((key-pressed "1")
     (when (not key-debounce)
     (set! key-debounce #t)
     (set! 1? (not 1?))))
    ((key-pressed "2")
     (when (not key-debounce)
     (set! key-debounce #t)
     (set! 2? (not 2?))))
    ((key-pressed "3")
     (when (not key-debounce)
     (set! key-debounce #t)
     (set! 3? (not 3?))))
    ((key-pressed "4")
     (when (not key-debounce)
     (set! key-debounce #t)
     (set! 4? (not 4?))))
    ((key-pressed "5")
     (when (not key-debounce)
     (set! key-debounce #t)
     (set! 5? (not 5?))))
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
    (else (set! key-debounce #f))))

(every-frame
    (begin
        (check-key)
        (render))) 
