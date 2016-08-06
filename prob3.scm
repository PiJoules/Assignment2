; Leonard Chan
; Usage:
; 1. Launch mit-scheme
;    - $ mit-scheme
; 2. Load this file in the cli
;    - 1 ]=> (load "prob3.scm")
; The script contains various assertions and tests for testing the problem
; solutions. No error messages should appear of all tests run correctly and
; the code works.

; 4.4
; Utility functions for asserting
(define (reporterr msg)
    (newline)
    (display msg)
    (newline))

(define (assert msg b)
     ( if ( not b) (reporterr msg)))

(define (assert-true b)
  (assert "The given expression does not evaluate to True." b))
(define (assert-false b)
  (assert "The given expression does not evaluate to False." (not b)))

; Check if an argument evaluates to false
(define (eval-false arg)
    (cond
        ((equal? arg 'false) #t)
        ((equal? arg "false") #t)
        ((equal? arg '()) #t)
        ((equal? arg 0) #t)
        (else #f)
    )  
)
(define (eval-true arg)
  (not (eval-false arg))
)

(assert-true (eval-false "false"))
(assert-true (eval-false '()))
(assert-true (eval-false 0))
(assert-false (eval-false "true"))
(assert-false (eval-false 1))
(assert-false (eval-false '(0)))

; Evaluate an and expression
(define (eval-and args)
  (if (null? args)
    #t  ; No more expressions
    (if (eval-false (car args))
      #f
      (eval-and (cdr args))
    )
  )
)

; Evaluate an orexpression
(define (eval-or args)
  (if (null? args)
    #f  ; No more expressions
    (if (eval-true (car args))
      #t
      (eval-or (cdr args))
    )
  )
)

(define (eval expr)
  (cond
    ((equal? (car expr) 'and) (eval-and (cdr expr)))
    ((equal? (car expr) 'or) (eval-or (cdr expr)))
    (else #t)
  )
)

; tests for 4.4
(assert-true (eval '(and)))
(assert-true (eval '(and true 1)))
(assert-false (eval '(and false)))
(assert-false (eval '(and false 2 true)))
(assert-false (eval '(and 1 true false)))

(assert-false (eval '(or)))
(assert-true (eval '(or true 1)))
(assert-false (eval '(or false)))
(assert-true (eval '(or false 2 true)))
(assert-true (eval '(or 1 true false)))

; 4.9
; Implementation of for construct
(define (eval-for start stop step expr)
  (if (start >= stop)
    #t  ; Default evaluation
    (eval-for (+ start step) stop step (eval expr))
  )
)

; 4.11
; Use a frames list instead of 2 lists of vars and vals
(define (lookup-variable-value var env)
   (define (env-loop env)
      (define (scan frames)
         (cond ((null? frames)
                 (env-loop (enclosing-environment env)))
                ((eq? var (cadr (car vars)))
                  (car (car vals)))
                 (else (scan (cdr frames)))))
       (if (eq? env the-empty-environment)
          (error "Unbound variable" var)
           (let ((frame (first-frame env)))
              (scan (frame-variables frame)
                     (frame-values frame)))))
    (env-loop env))

(define (set-variable-value! var val env)
   (define (env-loop env)
      (define (scan frames)
         (cond ((null? frames)
                 (env-loop (enclosing-environment env)))
                ((eq? var (cadr (car vars)))
                  (set-car! (car vals)))
                 (else (scan (cdr frames)))))
       (if (eq? env the-empty-environment)
          (error "Unbound variable -- SET!" var)
           (let ((frame (first-frame env)))
              (scan (frame-variables frame)
                     (frame-values frame)))))
    (env-loop env))

(define (define-variable! var val env)
   (let ((frame (first-frame env)))
       (define (scan frames)
          (cond ((null? frames) 
                 (add-binding-to-frame! var val frame))
                 ((eq? var (cadr (car vars)))
                   (set-car! (car vals)))
                  (else (scan (cdr frames)))))
       (scan (frame-variables frame))))

