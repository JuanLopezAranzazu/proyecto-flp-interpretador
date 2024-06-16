#lang racket/base

(require rackunit "../proyecto.rkt")

;Test 1: 
(define exp1 
  (scan&parse
    "
    let
    s = \"hola mundo cruel\"
    in
      string-length(s)
    "
  )
)
(define expected-exp1
  16
)

(check-equal? (eval-program exp1) expected-exp1)

;Test 2: 
(define exp2
  (scan&parse
    "
    let
    s = \"hola mundo cruel\"
    in
      elementAt(s, 5)
    "
  )
)
(define expected-exp2
  #\m
)

(check-equal? (eval-program exp2) expected-exp2)

;Test 3: 
(define exp3
  (scan&parse
    "
    let
    a = \"hola\"
    b = \"mundo\"
    c = \"cruel\"
    in
      concat(a, b, c)
    "
  )
)
(define expected-exp3
  "holamundocruel"
)

(check-equal? (eval-program exp3) expected-exp3)

