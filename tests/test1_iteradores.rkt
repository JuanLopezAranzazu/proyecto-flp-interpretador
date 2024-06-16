#lang racket/base

(require rackunit "../proyecto.rkt")

;Test 1: 
(define exp1 
  (scan&parse
    "let
      x = 0
      in
        begin
          for i from 0 until 10 by 1 do
            set x = (x + i);
          x
        end
    "
  )
)
(define expected-exp1
  45
)

(check-equal? (eval-program exp1) expected-exp1)


;Test 2: 
(define exp2 
  (scan&parse
    "let
      x = 5
      y = 0
      in
        begin
          for i from 2 until 12 by 2 do
            set x = (x + i);
          
          for i from 0 until 10 by 1 do
            set y = (y + i);
          (x + y)
        end
    "
  )
)
(define expected-exp2
  80
)

(check-equal? (eval-program exp2) expected-exp2)


;Test 3: 
(define exp3
  (scan&parse
    "let
      x = 0
      in
        begin
          while (x < 10) {
            set x = (x + 1)
          };
          x
        end
    "
  )
)
(define expected-exp3
  10
)

(check-equal? (eval-program exp3) expected-exp3)

;Test 4: 
(define exp4
  (scan&parse
    "let
      x = 6
      in
        begin
          while (x <= 10) {
            set x = (x + 1)
          };
          (x * 2)
        end
    "
  )
)
(define expected-exp4
  22
)

(check-equal? (eval-program exp4) expected-exp4)

