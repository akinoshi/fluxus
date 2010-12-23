; File: cube-3.scm
; ----------------
; A script for Lacking Sound Festival Listen 41
; performance. This is for workspace 3, which
; generates a recursive cubic sculpture.
;
; Key Bindings
; ------------
; 1: shrink all the movable cubes
; SPACE: manually shrink a movable cube
; f: (f)ree all the movable cubes
; a: (a)ctivate fixed cubes
; 
; Author: Akinori Kinoshita
; E-mail: art.akinoshi -at- gmail.com
; Date: Sun Nov 21 01:00:20 CST 2010

(require scheme/math)
(require scheme/class)

(clear)

(collisions 1)
(gravity (vector 0 0 0))

(light-diffuse 0 (vector 0.4 0.4 0.4))
(define l (make-light 'point 'free))
(light-diffuse l (vector 1 1 1))
(light-position l (vector 20 5 30))
(shadow-light l)

(hint-cast-shadow)

(define key-debounce #f)
(define ribbon? #f)
(define ribbons 0)
(define destroy-list '())
(define active? #f)
(define one #(0.91 0.51 0.07))
(define two #(0.34 0.79 0.84))
(define three #(0.43 0.49 0.5))
;(define one #(1 0 0))
;(define two #(0 1 0))
;(define three #(0 0 1))

(osc-destination "osc.udp://192.168.0.100:12000")
(osc-source "12001")

(define pos '(
; 1st order (1)
#(0 0 0)
; 2nd order (19)
#(-1 0 -1) #(-1 -1 0) #(-1 -1 1) #(0 -1 1) #(1 0 1)
#(0 1 -1) #(1 1 0) #(-1 1 -1) #(1 1 1) #(-1 -1 -1)
#(1 -1 1) #(1 1 -1) #(1 0 0) #(0 -1 0) #(0 0 -1)
#(1 -1 0) #(0 -1 -1) #(1 0 -1) #(1 -1 -1)
; 3rd order (61)
#(-2 -2 -2) #(-2 -1 -2) #(-2 0 -2) #(-2 1 -2) #(-2 2 -2)
#(-1 2 -2) #(0 2 -2) #(1 2 -2) #(2 2 -2) #(2 2 -1)
#(2 2 0) #(2 2 1) #(2 2 2) #(2 1 2) #(2 0 2)
#(2 -1 2) #(2 -2 2) #(1 -2 2) #(0 -2 2) #(-1 -2 2)
#(-2 -2 2) #(-2 -2 1) #(-2 -2 0) #(-2 -2 -1) #(2 0 0)
#(2 -1 0) #(2 1 0) #(2 1 -1) #(2 1 -2) #(2 1 1)
#(2 0 -1) #(2 0 -2) #(2 0 1) #(2 -1 1) #(2 -1 -1)
#(2 -1 -2) #(2 -2 0) #(2 -2 -1) #(2 -2 -2) #(2 -2 1)
#(1 -2 0) #(1 -2 -1) #(1 -2 -2) #(1 -2 1) #(0 -2 0)
#(0 -2 1) #(0 -2 -1) #(0 -2 -2) #(0 -1 -2) #(0 0 -2)
#(0 1 -2) #(-1 -2 -1) #(-1 -2 0) #(-1 -2 1) #(-1 -2 -2)
#(-1 -1 -2) #(-1 0 -2) #(-1 1 -2) #(1 0 -2) #(1 -1 -2)
#(1 1 -2)
))

(define active-pos '(
; 2nd order (7)
#(0 1 0) #(-1 0 0) #(0 0 1) #(-1 1 0) #(-1 1 1)
#(0 1 1) #(-1 0 1)
; 3rd order
#(-2 0 0) #(-2 0 -1) #(-2 0 1) #(-2 0 2) #(-2 -1 -1)
#(-2 -1 0) #(-2 -1 1) #(-2 -1 2) #(-2 1 -1) #(-2 1 0)
#(-2 1 1) #(-2 1 2) #(-2 2 -1) #(-2 2 0) #(-2 2 1)
#(-2 2 2) #(-1 2 -1) #(-1 2 0) #(-1 2 1) #(-1 2 2)
#(0 2 -1) #(0 2 0) #(0 2 1) #(0 2 2) #(1 2 -1)
#(1 2 0) #(1 2 1) #(1 2 2) #(-1 1 2) #(-1 0 2)
#(-1 -1 2) #(0 1 2) #(0 0 2) #(0 -1 2) #(1 1 2)
#(1 0 2) #(1 -1 2)
))

(define shrink-list (build-list (length active-pos) values))

(define (setup list i)
    (when (not (null? list))
        (begin
            (with-state
                (cond
                    ((= i 0) (colour one))
                    ((and (> i 0) (< i 20)) (colour two))
                    (else (colour three)))
                (translate (car list))
                (let ((obj (build-cube)))
                (set! destroy-list (cons obj destroy-list))
                (if active?
                    (active-box obj)
                    (passive-box obj)))
                ))
            (setup (cdr list) (+ i 1))))

(setup pos 0)

(define (get-pos)
    (let ([t (get-global-transform)])
        (vector (vector-ref t 12)
                (vector-ref t 13)
                (vector-ref t 14))))

(define ribbon%
    (class object%
        (init id_)
        
        (field [id id_])
 
        (define c (with-state
            (if (< id_ 7)
                (colour two)
                (colour three))
            (translate (list-ref active-pos id_))
            (scale 0.25)
            (build-cube)))

        ;(define c (build-cube))
        (active-box c)
        
        (define r (build-ribbon 64))
        
        (with-primitive r
            (hint-unlit)
            ;(colour #(1 0.25))
            (pdata-index-map!
                (lambda (i w)
                    ;(* .1 (cos (* .5 pi (/ i (pdata-size)))))
                    0.015
                    )
                "w"))

(with-primitive r
    (pdata-map! (lambda (p) (list-ref active-pos id)) "p"))
        
        (define (add-p np)
            (with-primitive r
                (pdata-index-map!
                    (lambda (i p)
                        (if (< i (- (pdata-size) 2))
                            (pdata-ref "p" (add1 i))
                            np))
                    "p")))
            
        (define/public (update)
            (with-primitive c
                (opacity 1))
            (add-p (with-primitive c
                        (get-pos)))
            (when (> 1 .2)
                0
                (kick c (vmul (crndvec) (flxrnd)))
                ;(twist c (vmul (crndvec) (flxrnd)))
                )
            (let ([p (with-primitive c
                        (get-pos))])
                (kick c (vmul p -.05))))
        
        (super-new)))

#;(define ribbons (build-list (length active-pos)
                    (lambda (x) (make-object ribbon% x))))

; tweener equation
(define (easeInExpo t b c d)
    (if (zero? t)
        b
        (+ (* c (expt 2 (* 10 (- (/ t d) 1)))) b)))

(define (easeOutExpo t b c d)
    (if (= t d)
        (+ b c)
        (+ (* c (+ (* -1 (expt 2 (* -10 (/ t d)))) 1)) b)))

; get size using tweener
(define (get-size)
    (let ((s (* (list-ref size cube-id)
                (easeOutExpo (- (* 1000 (time)) last-time) 0 1 1000))))
         (if (> s (* (list-ref size cube-id) 0.99))
            (list-ref size cube-id)
            s)))

(define particle%
    (class object%
        (init id_)

        (field [id id_])
        
        (define shrink? #f)
        (define last-time (* 1000 (time)))

        (define (get-scale)
            (let ((s (* 0.75 (easeInExpo (- (* 1000 (time)) last-time) 0 1 1000))))
            (if (> s 0.75)
                0.75
                s)))

        (define/public (setup)
            (set! shrink? #t)
            (set! last-time (* 1000 (time))))

        (define/public (update)
            (with-state
                (if (< id 7)
                    (colour two)
                    (colour three))
                (translate (list-ref active-pos id))
                (when shrink? (scale (- 1 (get-scale))))
                (draw-cube)))

        (super-new)))

(define particles (build-list (length active-pos)
                    (lambda (x) (make-object particle% x))))
                
(define (render)
    (for-each
        (lambda (r)
            (send r update))
        ribbons))

(define (render-particles)
    (for-each
        (lambda (p)
            (send p update))
        particles))

(define (check-key) (cond
    ((key-pressed "1")
        (when (not key-debounce)
            (set! key-debounce #t)
            (set! shrink-list '())
            (for-each (lambda (x) (send x setup)) particles)))
    ((key-pressed " ")
        (when (and (not key-debounce) (not (null? shrink-list)))
            (set! key-debounce #t)
            (let* ((i (random (length shrink-list)))
                   (j (list-ref shrink-list i)))
                 (send (list-ref particles j) setup)
                 (set! shrink-list (remove j shrink-list)))))
    ((key-pressed "f")
        (when (and (not key-debounce) (not ribbon?) (null? shrink-list))
            (set! key-debounce #t)
            (set! ribbons (build-list (length active-pos)
                (lambda (x) (make-object ribbon% x))))
            (set! ribbon? #t)
            (osc-send "/free" "f" (list 1))))
    ((key-pressed "a")
        (when (not key-debounce)
            (set! key-debounce #t)
            (destroy-all destroy-list)
            (set! active? #t)
            (setup pos 0)
            (osc-send "/activate" "f" (list 1))))
    (else (set! key-debounce #f))))

(define (destroy-all list)
    (when (not (null? list))
        (destroy (car list))
        (destroy-all (cdr list))))

(define (swing)
    (light-position l (vmul (vector (sin (time)) 0.2
                                    (cos (time))) 50)))

(define (check-osc)
    (when (osc-msg "/shrink")
        (when (not (null? shrink-list))
            (let* ((i (random (length shrink-list)))
                   (j (list-ref shrink-list i)))
                 (send (list-ref particles j) setup)
                 (set! shrink-list (remove j shrink-list))))))

(every-frame
    (begin
        (check-key)
        (check-osc)
        (if ribbon?
            (render)
            (render-particles))
        (swing)
    ))
