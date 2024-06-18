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

;Test 3:
(define exp3 
  (scan&parse
    "let
      x = true
      in
        begin
          switch (x) {
            case and((1 == 1), (2 == 2)): (2 pow 5)
            default: 0
          }
        end
    "
  )
)
(define expected-exp3
  32
)

(check-equal? (eval-program exp3) expected-exp3)

;Test 4:
(define exp4 
  (scan&parse
    "let
      x = list(1, 2, 3, 4, 5)
      in
        begin
          switch (x) {
            case 1: 1
            case empty?(x): 2
            default: if ((16 mod 2) == 0) { \"hola\" else \"mundo\" } 
          }
        end
    "
  )
)
(define expected-exp4
  "hola"
)

(check-equal? (eval-program exp4) expected-exp4)

