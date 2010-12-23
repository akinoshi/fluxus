; using (list) (car) (cdr) (list-ref)
(clear)
(define p '(1 2 2))
(translate (vector (car p) 0 0))
(build-cube)
(translate (vector (list-ref p 1) 0 0))
(build-cube)
(translate (vector (car (cdr (cdr p))) 0 0))
(build-cube)

; using (if)
(clear)
(define (change-colour)
    (if (> (mouse-x) (/ (vx (get-screen-size)) 2))
    (colour (vector 1 0 0))
    (colour (vector 0 1 0)))
    (draw-cube))
(every-frame (change-colour))

; using recursion (when)
(clear)
(define (draw n)
    (when (not (= n 0))
	(translate (vector 1.1 0 0))
	(draw-cube)
	(draw (- n 1))))
(every-frame (draw 10))

; using (for-each)
(clear)
(define position '(1 2 3 5 7))
(for-each
    (lambda (x)
        (with-state
        (translate (vector x 0 0))
        (build-cube)))
    position)

; using (for)
(clear)
(for [(i (in-range 3))]
    (with-state
    (translate (vector i i 0))
    (build-cube)))

; using (map) (pdata-map!)
(clear)
(with-primitive (build-particles 100)
    (pdata-map! (lambda (p) (grndvec)) "p")
    (pdata-map! (lambda (c) (vector 1 1 1)) "c"))