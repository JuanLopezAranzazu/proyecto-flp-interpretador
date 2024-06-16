#lang racket/base

(require rackunit "../proyecto.rkt")

;Test 1: 
(define exp1 
  (scan&parse
    "
    and((1 > 2), false)
    "
  )
)
(define expected-exp1
  #f
)

(check-equal? (eval-program exp1) expected-exp1)

;Test 2:
(define exp2 
  (scan&parse
    "
    or((1 > 2), false, true, (2 < 3))
    "
  )
)
(define expected-exp2
  #t
)

(check-equal? (eval-program exp2) expected-exp2)

;Test 3:
(define exp3 
  (scan&parse
    "
    let
      x = 4
      l = 2
      r = 3
      in
        if and((x >= l), (x <= r)) {
          1
        else 
          2
        }
    "
  )
)
(define expected-exp3
  2
)

(check-equal? (eval-program exp3) expected-exp3)

