#lang racket/base

(require rackunit "../proyecto.rkt")

;Test 1: 
(define exp1 
  (scan&parse
    "
    let
    x = 5
    in
      match x {
        numero(x) => x
        default => 0
      }
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
    x = list(1,2,3,4,5)
    in
      match x {
        x::xs => x
        default => 0
      }
    "
  )
)
(define expected-exp2
  1
)

(check-equal? (eval-program exp2) expected-exp2)

;Test 3: 
(define exp3
  (scan&parse
    "
    let
    x = list(1,2,3,4,5)
    in
      match x {
        x::xs => xs
        default => 0
      }
    "
  )
)
(define expected-exp3
  '(2 3 4 5)
)

(check-equal? (eval-program exp3) expected-exp3)


;Test 4: 
(define exp4
  (scan&parse
    "
    let
    x = array(1,2,3,4,5)
    in
      match x {
        x::xs => xs
        array(x,y) => list(x,y)
        default => 0
      }
    "
  )
)
(define expected-exp4
  '(1 #(2 3 4 5))
)

(check-equal? (eval-program exp4) expected-exp4)

;Test 5: 
(define exp5
  (scan&parse
    "
    let
    x = empty
    in
      match x {
        x::xs => xs
        array(x,y) => list(x,y)
        empty => (2 * 5)
        default => 0
      }
    "
  )
)
(define expected-exp5
  10
)

(check-equal? (eval-program exp5) expected-exp5)

;Test 6:
(define exp6
  (scan&parse
    "
    let
    x = \"hola mundo\"
    y = \"cruel\"
    in
      match x {
        x::xs => xs
        array(x,y) => list(x,y)
        empty => -1
        cadena(x) => concat(x,y)
        default => 0
      }
    "
  )
)
(define expected-exp6
  "hola mundocruel"
)

(check-equal? (eval-program exp6) expected-exp6)

;Test 7:
(define exp7
  (scan&parse
    "
    let
    x = true
    a = and((4 >= 2), (5 < 3))
    in
      match x {
        x::xs => xs
        array(x,y,z) => list(x,y,z)
        empty => -1
        cadena(x) => concat(x,y)
        boolean(x) => not(a)
        default => 0
      }
    "
  )
)
(define expected-exp7
  #t
)

(check-equal? (eval-program exp7) expected-exp7)

;Test 8:
(define exp8
  (scan&parse
    "
    let
    x = list(2,4,6,8,10)
    in
      match x {
        array(x,y,z) => list(x,y,z)
        empty => -1
        cadena(x) => concat(x,y)
        boolean(x) => not(a)
        x::xs => list(x,xs)
        default => 0
      }
    "
  )
)
(define expected-exp8
  '(2 (4 6 8 10))
)

(check-equal? (eval-program exp8) expected-exp8)

