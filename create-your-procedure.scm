; (append)
(define (my-append ls0 ls1)
  (if (null? ls1)
      ls1
      (cons (car ls0) (append (cdr ls0) ls1))))

; (last)
(define (my-last list)
  (if (= (length list) 1)
      (car list)
      (my-last (cdr list))))

; (map)
(define (my-map procedure list)
  (if (null? list)
      empty
      (cons (procedure (car list))
            (my-map procedure (cdr list)))))

; (reverse)
(define (my-reverse list)
  (my-reverse-rec list empty))
(define (my-reverse-rec ls0 ls1)
  (if (null? ls0)
      ls1
      (my-reverse-rec (cdr ls0) (cons (car ls0) ls1))))

; (list-ref)
(define (my-list-ref list x)
  (if (= x 0)
      (car list)
      (my-list-ref (cdr list) (- x 1))))

; (member)
(define (my-member x list)
  (cond
    ((null? list) #f)
    ((eqv? x (car list)) list)
    (else (my-member x (cdr list)))))

; (length)
(define (my-length list)
  (if (null? list)
      0
      (+ 1 (my-length (cdr list)))))

; (make-list)
(define (my-make-list i v)
  (if (= i 0)
      '()
      ((lambda (x) (cons v x))
       (my-make-list (- i 1) v))))

; (remove)
(define (my-remove v list)
  (if (null? list)
      '()
      (let ((x (car list)))
        ((if (eqv? x v)
            (lambda (y) y)
            (lambda (y) (cons x y)))
        (my-remove v (cdr list))))))

; (take-right)
(define (my-take-right list i)
  (my-take-right-rec (reverse list) empty i))
(define (my-take-right-rec ls0 ls1 i)
  (if (= i 0)
      ls1
      (my-take-right-rec (cdr ls0) (cons (car ls0) ls1) (- i 1))))
