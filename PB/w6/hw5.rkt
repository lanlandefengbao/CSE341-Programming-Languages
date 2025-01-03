;; Programming Languages, Homework 5

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for MUPL programs - Do NOT change
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct int  (num)    #:transparent)  ;; a constant number, e.g., (int 17)
(struct add  (e1 e2)  #:transparent)  ;; add two expressions
(struct ifgreater (e1 e2 e3 e4)    #:transparent) ;; if e1 > e2 then e3 else e4
(struct fun  (nameopt formal body) #:transparent) ;; a recursive(?) 1-argument function
(struct call (funexp actual)       #:transparent) ;; function call
(struct mlet (var e body) #:transparent) ;; a local binding (let var = e in body) 
(struct apair (e1 e2)     #:transparent) ;; make a new pair
(struct fst  (e)    #:transparent) ;; get first part of a pair
(struct snd  (e)    #:transparent) ;; get second part of a pair
(struct aunit ()    #:transparent) ;; unit value -- good for ending a list
(struct isaunit (e) #:transparent) ;; evaluate to 1 if e is unit else 0

;; a closure is not in "source" programs but /is/ a MUPL value; it is what functions evaluate to
(struct closure (env fun) #:transparent) 

;; Problem 1
(define (racketlist->mupllist lst)
  (if (= 0 (length lst))
      (aunit)
      (apair (car lst) (racketlist->mupllist (cdr lst)))))

;; CHANGE (put your solutions here)

;; Problem 2
(define (mupllist->racketlist mlst)
  (if (aunit? mlst)
      null
      (cons (apair-e1 mlst) (mupllist->racketlist (apair-e2 mlst)))))
  
;; lookup a variable in an environment
;; Do NOT change this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car (car env)) str) (cdr (car env))]
        [#t (envlookup (cdr env) str)]))

;; Do NOT change the two cases given to you.  
;; DO add more cases for other kinds of MUPL expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e) 
         (envlookup env (var-string e))]
        [(add? e) 
         (let ([v1 (eval-under-env (add-e1 e) env)]
               [v2 (eval-under-env (add-e2 e) env)])
           (if (and (int? v1)
                    (int? v2))
               (int (+ (int-num v1) 
                       (int-num v2)))
               (error "MUPL addition applied to non-number")))]
        ;; CHANGE add more cases here
        [(int? e) e]
        [(ifgreater? e)
         (let ([v1 (eval-under-env (ifgreater-e1 e) env)]
               [v2 (eval-under-env (ifgreater-e2 e) env)])
           (if (and (int? v1) (int? v2))
               (if (> (int-num v1) (int-num v2))
                   (eval-under-env (ifgreater-e3 e) env)
                   (eval-under-env (ifgreater-e4 e) env))
               (error "MUPL ifgreater applied to non-number")))]
        [(mlet? e) ;(var s, e e, body e)
         (let ([v (eval-under-env (mlet-e e) env)])
           (eval-under-env (mlet-body e) (cons (cons (mlet-var e) v) env)))]
        [(fun? e) ;(nameopt s, formal s, body e)
         (closure env e)] ;stores the func itself & the env the func is constructed on (without the input argument)
        ; STILL SOMETHING WRONG, BUT CLOSE
        [(call? e) ;(funexp e, actual e)
         (let ([cl (eval-under-env (call-funexp e) env)])
           (if (closure? cl)
               (let ([fname (fun-nameopt (closure-fun cl))]
                     [arg_input (eval-under-env (call-actual e) env)]
                     [arg (fun-formal (closure-fun cl))]
                     [fbody (fun-body (closure-fun cl))]
                     [fenv (closure-env cl)])
                 (if (equal? fname #f) ;non-recursive fun 
                     (eval-under-env fbody (cons (cons arg arg_input) fenv))
                     ;fun that may be called recursively
                     (eval-under-env fbody (cons
                                            (cons fname cl)
                                            (cons (cons arg arg_input) fenv)))))
               (error  "MUPL call applied to non-closure")))]
        [(apair? e)
         (apair (eval-under-env (apair-e1 e) env) (eval-under-env (apair-e2 e) env))]
        [(fst? e)
         (let ([res (eval-under-env e env)])
           (if (apair? res)
               (apair-e1 res)
               (error "MUPL fst applied to non-pair")))]
        [(snd? e)
         (let ([res (eval-under-env e env)])
           (if (apair? res)
               (apair-e2 res)
               (error "MUPL snd applied to non-pair")))]
        [(aunit? e) e]
        [(isaunit? e)
         (let ([res (eval-under-env (isaunit-e e) env)])
           (if (aunit? res)
               (int 1)
               (int 0)))]         
        [#t (error (format "bad MUPL expression: ~v" e))]))
; test case for call
#|
(equal? (eval-under-env
           (call
            (fun "sum" "x"
               (ifgreater (int 10) (var "x")
                          (add (var "x") (call (var "sum") (add (var "x") (int 1))))
                          (int 10)))
            (int 1))
           '())
          (int 55))
|#
;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))
        
;; Problem 3

(define (ifaunit e1 e2 e3) (ifgreater (isaunit e1) (int 0) e2 e3))

(define (mlet* lstlst e2)
  (if (null? lstlst)
      e2
      (mlet (car (car lstlst)) (cdr (car lstlst))
            (mlet* (cdr lstlst) e2))))

(define (ifeq e1 e2 e3 e4) (mlet "_x" e1
                                 (mlet "_y" e2
                                       (ifgreater (var "_x") (var "_y") e4
                                                  (ifgreater (var "_y") (var "x") e4 e3)))))
                                                  

;; Problem 4

(define mupl-map (fun "h" "f_customized"
                      (fun "g" "mlst" (ifaunit (var "mlst")
                                              (aunit)
                                              (apair (call (var "f_customized") (fst (var "mlst")))
                                                     (call (var "g") (snd (var "mlst"))))))))
                                    

(define mupl-mapAddN 
  (mlet "map" mupl-map
        "CHANGE (notice map is now in MUPL scope)"))

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make
;; auto-grading and peer assessment more difficult, so
;; copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
