
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define ones (lambda () (cons 1 ones)))

(define (sequence l h gap)
  (if (> l h)
      null
      (cons l (sequence (+ l gap) h gap))))

(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))

(define (list-nth-mod lst n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(= (length lst) 0) (error "list-nth-mod: empty list")]
        [#t (car (list-tail lst (remainder n (length lst))))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (let ([cur (s)])
        (cons (car cur) (stream-for-n-steps (cdr cur) (- n 1))))))

(define funny-number-stream
  (letrec ([helper (lambda (x)
                    (if (or (< x 5) (not (= (remainder x 5) 0)))
                        (cons x (lambda () (helper (+ x 1))))
                        (cons (- x) (lambda () (helper (+ x 1))))
                        ))])
    (lambda () (helper 1))))

(define dan-then-dog
  (letrec ([helper (lambda (x1 x2)
                     (cons x1 (lambda () (helper x2 x1))))])
    (lambda () (helper "dan.jpg" "dog.jpg"))
    ))

(define (stream-add-zero s)
  (let ([pair (s)])
    (lambda () (cons (cons 0 (car pair)) (lambda () (stream-add-zero (cdr pair)))))
    ))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n m)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys m))
                                (lambda () (f (+ n 1) (+ m 1)))
                                ))])
    (lambda ()(f (length xs) (length ys)))
    ))
                                                           
(define (vector-assoc v vec)
  (letrec ([l (vector-length vec)]
           [f (lambda (v vec pos)
                (if (= pos l)
                    #f
                    (let ([cur (vector-ref vec pos)])
                      (if (not (pair? cur))
                          (f v vec (+ pos 1))
                          (if (equal? (car cur) v)
                              cur
                              (f v vec (+ pos 1))
                            )))))])
    (f v vec 0)))

(define (cached-assoc xs n)
  (let ([cache (make-vector n #f)]
        [idx 0])
    (lambda (v)
      (let ([ans (vector-assoc v cache)])
        (if ans
            ans
            (let ([ans (assoc v xs)])
              (if (equal? ans #f)
                  ans
                  (if (< idx n)
                      (begin (vector-set! cache idx ans)
                             (set! idx (+ idx 1))
                             ans)
                      (begin (set! idx 0)
                             (vector-set! cache 0 ans)
                             ans)
                      ))))))))     
          