#lang racket
(provide (all-defined-out))

(define (my-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (my-append (cdr xs) ys))))

(define (triple x)
  (letrec ([y (+ x 2)]
           [f (lambda (z) (+ z y w x))]
           [w (+ x 7)])
    (f -9)))

(define lst (list 1 2 3))

(define x 1)
(define y x)
(define (f z) (+ x z))
(define (g p)
  (let ([x x])
    (+ p x)))
(set! x 2)