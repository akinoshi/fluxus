; File: find-the-treasure-r5.scm
; ------------------------------
; A simple Fluxus game to find the
; treasure out of random located cubes
; (fade out feature was added).
;
; Author: Akinori Kinoshita
; E-mail: art.akinoshi -at- gmail.com
; Date: Fri Sep 10 23:23:00 CST 2010
;    Sat Sep 11 10:35:31 CST 2010

(clear)
(reset-camera)
(clear-colour (vector 0 0 0))
(hint-wire)
(backfacecull 0)

(osc-destination "osc.udp://localhost:12000")

(define size 50)
(define random-id (round (* size (flxrnd))))
(define treasure-id 0)
(define game-over-id 0)
(define obj-list '())
(define remove-list '())

; Generate random located cubes and a treasure hidden inside the one of the cubes
(define (build-cubes n)
    (when (not (zero? n))
        (translate (grndvec))
        (if (= n random-id)
            (begin
            (with-state
                (colour (vector 1 0 0))
                (scale 0.5)
                (set! treasure-id (build-cube)))
            (set! game-over-id (build-cube)))
            (build-cube))
        (build-cubes (- n 1))))

(build-cubes size)

; Render explosion of 6 sides of cubes except the treasure
(define (render)
    (for-each (lambda (id)
        (when (not (= id treasure-id))
            (with-primitive id
                (pdata-map!
                    (lambda (p n) (vadd p (vmul n 0.05))) "p" "n")
        ; Fade out the colour of cubes
                (let* ([a (vector-ref (pdata-ref "p" 0) 2)]
                       [b (+ 1 (* 0.25 a))])
                    (colour (vector 1 1 1 b))
                    (wire-colour (vector 1 1 1 b))
                    (when (< b 0)
                        (if (= id game-over-id)
                        (game-over)
                        (when (not (member id remove-list))
                            (set! remove-list (cons id remove-list)))))))))
    obj-list))

; Append the primitive id into the list for explosion
(define (check-mouse)
    (when (mouse-button 1)
        (let ((s (select (mouse-x) (mouse-y) 2)))
            (when (not (zero? s))
                (when (not (member s obj-list))
                    (set! obj-list (cons s obj-list))
                    (osc-send "/bang" "f" (list 0)))))))

; Remove cubes after fading out completely
(define (remove-cubes)
    (for-each
        (lambda (id)
            (begin
            (destroy id)
            (set! obj-list (remove id obj-list))))
        remove-list)
    (set! remove-list '()))

(define (game-over)
    (clear)
    ;(clear-colour (vector 0.5 0.5 0))
    (reset-camera)
    (scale 0.5)
    (translate (vector -12 0 0))
    (build-type "Bitstream-Vera-Sans-Mono.ttf" "Game Over"))

(every-frame
    (begin
        (check-mouse)
        (render)
        (remove-cubes)))
