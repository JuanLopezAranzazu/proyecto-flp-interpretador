#lang racket/base

(require rackunit "../proyecto.rkt")

;Test 1: 
(define exp1 
  (scan&parse
    "
    let
    f = func(x,y) (x + y)
    in
      call f(2,3)
    "
  )
)
(define expected-exp1
  5
)

(check-equal? (eval-program exp1) expected-exp1)

;Test 2: 
(define exp2 
  (scan&parse
    "
    let
    a = 5
    b = 3
    c = false
    f = func(x,y,z) if z { (x + y) else (x - y) }
    in
      (call f(a,b,c) + call f(a,b,not(c)))
    "
  )
)
(define expected-exp2
  10
)

(check-equal? (eval-program exp2) expected-exp2)

;Test 3: 
(define exp3 
  (scan&parse
    "
    letrec
    f(x) = if (x > 0) { (x + call f((x - 1))) else 0 }
    in
      call f(10)
    "
  )
)
(define expected-exp3
  55
)

(check-equal? (eval-program exp3) expected-exp3)
