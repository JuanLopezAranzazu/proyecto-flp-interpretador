#lang racket/base

(require rackunit "../proyecto.rkt")

;Test 1: 
(define exp1 
  (scan&parse
    "let
      a = 5
      in
        if (a > 3) {
          1
        else 
          2
        }
    "
  )
)
(define expected-exp1
  1
)

(check-equal? (eval-program exp1) expected-exp1)

;Test 2:
(define exp2
  (scan&parse
    "let
      a = 5
      in
        if (a == 3) {
          (a + 1)
        else 
          (a - 1)
        }
    "
  )
)
(define expected-exp2
  4
)

(check-equal? (eval-program exp2) expected-exp2)
