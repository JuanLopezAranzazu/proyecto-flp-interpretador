#lang racket/base

(require rackunit "../proyecto.rkt")

;Test 1: 
(define exp1 
  (scan&parse
    "
    let
    t = array(1,2,3,4,5)
    in
      length(t)
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
    t = array(1,2,3,4,5)
    in
      index(t,2)
    "
  )
)
(define expected-exp2
  3
)

(check-equal? (eval-program exp2) expected-exp2)

;Test 3: 
(define exp3
  (scan&parse
    "
    let
    k = array(1,2,3,4,5,6,7,8,9,10)
    in
      slice(k, 2, 5)
    "
  )
)
(define expected-exp3
  #(3 4 5 6)
)

(check-equal? (eval-program exp3) expected-exp3)

;Test 4: 
(define exp4
  (scan&parse
    "
    let
    k = array(1,2,3,4,5,6,7,8,9,10)
    in
      setlist(k, 2, 10)
    "
  )
)
(define expected-exp4
  #(1 2 10 4 5 6 7 8 9 10)
)

(check-equal? (eval-program exp4) expected-exp4)

;Test 5:
(define exp5
  (scan&parse
    "
    let
    k = array(1,2,3,4,5,6,7,8,9,10)
    in
      setlist(slice(k, 2, 5), 0, 99)
    "
  )
)
(define expected-exp5
  #(99 4 5 6)
)

(check-equal? (eval-program exp5) expected-exp5)

;Test 6: 
(define exp6
  (scan&parse
    "
    let
    k = array(1,2,3,4,5,6,7,8,9,10)
    in
      slice(k, 7, 9)
    "
  )
)
(define expected-exp6
  #(8 9 10)
)

(check-equal? (eval-program exp6) expected-exp6)