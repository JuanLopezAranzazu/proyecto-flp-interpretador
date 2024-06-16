#lang racket/base

(require rackunit "../proyecto.rkt")

;Test 1: 
(define exp1 
  (scan&parse
    "
    let
    l = cons(1 cons(2 cons(3 empty)))
    in
      list(first(l), rest(l), empty?(rest(l)))
    "
  )
)
(define expected-exp1
  '(1 (2 3) #f)
)

(check-equal? (eval-program exp1) expected-exp1)

;Test 2:
(define exp2
  (scan&parse
    "
    let
      l1 = cons(1 cons(2 cons(3 empty)))
      l2 = cons(4 cons(5 cons(6 empty)))
    in
      list(first(l1), rest(l1), empty?(rest(l1)), first(l2), rest(l2), empty?(rest(l2)))
    "
  )
)
(define expected-exp2
  '(1 (2 3) #f 4 (5 6) #f)
)

(check-equal? (eval-program exp2) expected-exp2)
