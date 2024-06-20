#lang racket/base

(require rackunit "../proyecto.rkt")

;Test 1: 
(define exp1 
  (scan&parse
    "
    struct perro { nombre edad color }
    let 
    t = new perro(\"lucas\", 2 ,\"verde\") 
    in 
      get t.color
    "
  )
)
(define expected-exp1
  "verde"
)

(check-equal? (eval-program exp1) expected-exp1)

;Test 2: 
(define exp2 
  (scan&parse
    "
    struct perro { nombre edad color }
    let 
    t = new perro(\"lucas\" , 2 , \"verde\") 
    in 
      begin 
        set-struct t.nombre = \"pepe\"; 
        get t.nombre 
      end
    "
  )
)
(define expected-exp2
  "pepe"
)

(check-equal? (eval-program exp2) expected-exp2)

;Test 3: 
(define exp3 
  (scan&parse
    "
    struct perro { nombre edad color }
    let 
    t = new perro(\"lucas\" , 2 , \"verde\") 
    in 
      begin 
        set-struct t.color = \"rojo\"; 
        let
          x = get t.color
        in
          x 
      end
    "
  )
)
(define expected-exp3
  "rojo"
)

(check-equal? (eval-program exp3) expected-exp3)
