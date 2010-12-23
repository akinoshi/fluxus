; File: x'mas.scm
; ---------------
; A Fluxus script to draw a Christmas
; gift box and rotating snow.
;
; Author: Akinori Kinoshita
; E-mail: art.akinoshi -at- gmail.com
; Date: Sat Dec 18 21:01:53 CST 2010

(clear)

(define action (list translate rotate scale))

(define input (list
; +x stripe
(list (vector 0.51 0 0)
      (vector 0 90 0)
      (vector 0.15 1.02 1))
; -x stripe
(list (vector -0.51 0 0)
      (vector 0 90 0)
      (vector 0.15 1.02 1))
; +z stripe
(list (vector 0 0 0.51)
      (vector 0 0 0)
      (vector 0.15 1.02 1))
; -z stripe
(list (vector 0 0 -0.51)
      (vector 0 0 0)
      (vector 0.15 1.02 1))
; +y vertical stripe
(list (vector 0 0.51 0)
      (vector 90 0 0)
      (vector 0.15 1.02 1))
; +y horizontal stripe
(list (vector 0 0.51 0)
      (vector 90 0 0)
      (vector 1.02 0.15 1))
; -y vertical stripe
(list (vector 0 -0.51 0)
      (vector 90 0 0)
      (vector 0.15 1.02 1))
; -y horizontal stripe
(list (vector 0 -0.51 0)
      (vector 90 0 0)
      (vector 1.02 0.15 1))))

(define (build-stripe ls0 ls1)
    (when (not (null? ls0))
        (colour (vector 1 1 1))
        ((car ls0) (car ls1))
        (build-stripe (cdr ls0) (cdr ls1))))

(define (build-stripes-rec ls0 ls1)
    (when (not (null? ls1))
        (with-state
            (build-stripe ls0 (car ls1))
            (build-plane))
        (build-stripes-rec ls0 (cdr ls1))))

(define (build-stripes)
    (build-stripes-rec action input))

#;(with-state
    (hint-origin)
    (build-cube))

; ribbon
(with-primitive (build-ribbon 9)
    (hint-unlit)
    (colour (vector 1 1 1))
    (pdata-map! (lambda (w) 0.05) "w")
    (pdata-set! "p" 0 (vector 0.25 0.61 0.25))
    (pdata-set! "p" 1 (vector 0 0.51 0))
    (pdata-set! "p" 2 (vector 0.25 0.61 -0.25))
    (pdata-set! "p" 3 (vector 0.25 0.81 -0.25))
    (pdata-set! "p" 4 (vector 0 0.51 0))
    (pdata-set! "p" 5 (vector -0.25 0.61 -0.25))
    (pdata-set! "p" 6 (vector -0.25 0.81 -0.25))
    (pdata-set! "p" 7 (vector 0 0.51 0))
    (pdata-set! "p" 8 (vector -0.25 0.61 0.25)))

; rid
(define (build-rid)
    (with-state
        (colour (vector 0 0.5))
        (translate (vector 0 0.3 0))
        (rotate (vector 90 0 0))
        (scale (vector 1.01 1.01 0.01))
        (build-cube)))

(with-state
    ;(hint-origin)
    (colour (vector 1 0 0))
    (build-cube))

(build-stripes)

(build-rid)

(define particles (build-particles 100))

(with-primitive particles
    (pdata-map! (lambda (c) (vector 1 1 1)) "c")
    (pdata-map! (lambda (p) (vmul (grndvec) 5)) "p"))

(every-frame
    (with-primitive particles
        (rotate (vector 0 1 0))))
