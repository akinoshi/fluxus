; File: 16-harmonic-cubes.scm
; ---------------------------
; A simple Fluxus script to demonstrate the
; use of harmonics from the incoming sound.
;
; Author: Akinori Kinoshita
; E-mail: art.akinoshi -at- gmail.com
; Date: Sat Sep 11 00:47:01 CST 2010

(clear)
(gain 0.05)
(hint-unlit)
(hint-wire)
(wire-colour (vector 1 0 1))
(line-width 2)

; Prepare a list to set random colour for each cube
(define colour-list
    (build-list 16 (lambda (x) (rndvec))))

(define particles (build-particles 100))

(with-primitive particles
    (scale 5)
    ; Add the velocity pdata
    (pdata-add "vel" "v")
    ; Initialize all the velocities
    (pdata-map! (lambda (vel) (vmul (grndvec) 0.01)) "vel")
    ; Initialize all the colours
    (pdata-map! (lambda (c) (vector 1 0 1)) "c"))

(define (reset-particles)
    (with-primitive particles
        (pdata-map! (lambda (p) (vector 0 0 0)) "p")))

(define (render n)
    (cond
        ((zero? n) 0)
        (else
            (if (> (gh n) 120)
                (reset-particles)
                0)
            (with-state
		; Pick a colour from the colour list
                (colour (list-ref colour-list (- n 1)))
		; Adjust the height so that the cubes have same base
                (translate (vector n (* (- (gh n) 1) 0.5) 0))
		; Scale the cubes according to the harmonic values
                (scale (vector 1 (+ (gh n) 1) 1))
                (draw-cube))
            (render (- n 1)))))

(define (render-particles)
    (with-primitive particles
	; Update the positions
        (pdata-map! vadd "p" "vel")))

(translate (vector -8 0 0))

(every-frame
    (begin
        (render 16)
        (render-particles)))
