#lang racket/base

(require rackunit "../proyecto.rkt")

;Test 1: 
(define exp1 
  (scan&parse
    "let
      x = 0
      in
        begin
          switch (x) {
            case 1: 1
            case 2: 2
            default: 3
          }
        end
    "
  )
)
(define expected-exp1
  3
)

(check-equal? (eval-program exp1) expected-exp1)

;Test 2:
(define exp2 
  (scan&parse
    "let
      x = 2
      in
        begin
          switch (x) {
            case 1: (x + 1)
            case 2: (x + 2)
            default: (x + 3)
          }
        end
    "
  )
)
(define expected-exp2
  4
)

(check-equal? (eval-program exp2) expected-exp2)
