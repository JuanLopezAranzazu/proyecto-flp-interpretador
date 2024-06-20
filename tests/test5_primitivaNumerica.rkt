#lang racket/base

(require rackunit "../proyecto.rkt")

;Test 1: 
(define exp1 
  (scan&parse
    "
    let
    x = b1010
    y = b1100
    in
      (x + y)
    "
  )
)
(define expected-exp1
  "b10110"
)

(check-equal? (eval-program exp1) expected-exp1)

;Test 2:
(define exp2
  (scan&parse
    "
    let
    x = 0x77
    y = 0x3
    in
      (x + y)
    "
  )
)
(define expected-exp2
  "0x102"
)

(check-equal? (eval-program exp2) expected-exp2)

;Test 3:
(define exp3
  (scan&parse
    "
    let
    x = hx100
    y = hx2
    in
      (x - y)
    "
  )
)
(define expected-exp3
  "hxFE"
)

(check-equal? (eval-program exp3) expected-exp3)

;Test 4:
(define exp4
  (scan&parse
    "
    let
    x = 123.2
    y = 1.2
    in
      (x - y)
    "
  )
)
(define expected-exp4
  122.0
)

(check-equal? (eval-program exp4) expected-exp4)

;Test 5:
(define exp5
  (scan&parse
    "
    let
    x = hxFF
    y = hx10
    z = -hx2
    a = -hxD
    in
      (a + (z - (x + y)))
    "
  )
)
(define expected-exp5
  "-hx11E"
)

(check-equal? (eval-program exp5) expected-exp5)

;Test 6:
(define exp6
  (scan&parse
    "
    let
    x = 0x400
    y = 0x6
    in
      (x mod y)
    "
  )
)
(define expected-exp6
  "0x4"
)

(check-equal? (eval-program exp6) expected-exp6)

;Test 7:
(define exp7
  (scan&parse
    "
    let
    x = b101101
    y = b11
    in
      (x pow y)
    "
  )
)
(define expected-exp7
  "b10110001111110101"
)

(check-equal? (eval-program exp7) expected-exp7)

;Test 8:
(define exp8
  (scan&parse
    "
    let
    x = b1010
    y = -b1100
    in
      (x - y)
    "
  )
)
(define expected-exp8
  "b10110"
)

(check-equal? (eval-program exp8) expected-exp8)
