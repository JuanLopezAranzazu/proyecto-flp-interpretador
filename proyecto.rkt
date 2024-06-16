#lang eopl

(define lexica
'((white-sp
   (whitespace) skip)
  (comment
   ("//" (arbno (not #\newline))) skip)
  (identificador
   (letter (arbno (or letter digit "?"))) symbol)
  (digitoBinario
   ("b" (or "0" "1") (arbno (or "0" "1"))) string)
  (digitoBinario
   ("-" "b" (or "0" "1") (arbno (or "0" "1"))) string)
  (digitoDecimal
   (digit (arbno digit)) number)
  (digitoDecimal
   ("-" digit (arbno digit)) number)
  (digitoOctal
   ("0x" (or "0" "1" "2" "3" "4" "5" "6" "7")(arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoOctal
   ("-" "0x" (or "0" "1" "2" "3" "4" "5" "6" "7") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7"))) string)
  (digitoHexadecimal
   ("hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string)
  (digitoHexadecimal
   ("-" "hx" (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F") (arbno (or "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))) string) 
  (flotante
   (digit (arbno digit) "." digit (arbno digit)) number)
  (flotante
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  ))

(define gramatica
  '(
    (programa ((arbno struct-decl) expresion) a-programa)
    (expresion (bool-expresion) bool-exp)
    (expresion (identificador) var-exp)
    (expresion (numero-exp) num-exp)    
    ;;; (expresion ("\"" identificador (arbno identificador) "\"") cadena-exp)
    (expresion (var-decl) decl-exp)

    ;;Listas y arrays
    ;;; (expresion ("list" "(" (separated-list expresion ",") ")") lista-exp)
    ;;; (expresion ("cons" "(" expresion expresion ")") cons-exp)
    ;;; (expresion ("empty") empty-list-exp)
    ;;; (expresion ("array" "(" (separated-list expresion ",") ")") array-exp)

    ;;Expresion primitivas
    ;;Primitiva numerica
    (expresion ("(" expresion primitiva expresion ")") prim-num-exp)
    ;;Primitiva booleana
    (expresion (primitivaBooleana "(" (separated-list expresion ",") ")") prim-bool-exp)
    ;;; ;;Primitiva listas
    ;;; (expresion (primitivaListas "(" expresion ")") prim-list-exp)
    ;;; ;;Primitiva array
    ;;; (expresion (primitivaArray "(" (separated-list expresion ",") ")") prim-array-exp)
    ;;; ;;Primitiva de cadenas
    ;;; (expresion (primitivaCadena "(" (separated-list expresion ",") ")") prim-cad-exp)


    ;;Condicionales
    (expresion ("if" expresion "{" expresion "else" expresion "}") if-exp)


    ;;Iteradores
    (expresion ("for" identificador "from" expresion "until" expresion "by" expresion "do" expresion) for-exp)
    (expresion ("while" expresion "{" expresion "}") while-exp)

    ;;Switch
    (expresion ("switch" "(" expresion ")" "{" (arbno "case" expresion ":" expresion) "default" ":" expresion "}") switch-exp)

    ;;Secuenciación y asignación
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("set" identificador "=" expresion) set-exp)

    ;;Funciones
    ;;; (expresion ("func" "(" (separated-list identificador ",") ")" expresion) func-exp)
    ;;; (expresion ("call" expresion "(" (separated-list expresion ",") ")") call-exp)

    ;;Instanciación y uso de estructuras
    ;;; (expresion ("new" identificador "(" (separated-list expresion ",") ")") new-struct-exp)
    ;;; (expresion ("get" expresion "." identificador) get-struct-exp)
    ;;; (expresion ("set-struct" expresion "." identificador "=" expresion) set-struct-exp)

    ;;Reconocimiento de patrones
    ;;; (expresion ("match" expresion "{" (arbno regular-exp "=>" expresion) "}") match-exp)

    ;;Numero-exp
    (numero-exp (digitoDecimal) decimal-num)
    (numero-exp (digitoOctal) octal-num)
    (numero-exp (digitoBinario) bin-num)
    (numero-exp (digitoHexadecimal) hex-num)
    (numero-exp (flotante) float-num)
    
    ;;Bool-exp
    (bool-expresion ("true") true-exp)
    (bool-expresion ("false") false-exp)

    ;;primitivas numéricas
    (primitiva ("+") sum-prim)
    (primitiva ("-") minus-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("mod") mod-prim)
    (primitiva ("pow") elevar-prim)
    (primitiva ("<") menor-prim)
    (primitiva (">") mayor-prim)
    (primitiva ("<=") menorigual-prim)
    (primitiva (">=") mayorigual-prim)
    (primitiva ("!=") diferente-prim)
    (primitiva ("==") igual-prim)

    ;;primitiva booleana
    (primitivaBooleana ("and") and-prim)
    (primitivaBooleana ("or") or-prim)
    (primitivaBooleana ("xor") xor-prim)
    (primitivaBooleana ("not") not-prim)

    ;;Primitiva listas
    ;;; (primitivaListas ("first") first-primList)
    ;;; (primitivaListas ("rest") rest-primList)
    ;;; (primitivaListas ("empty?") empty-primList)

    ;;Primitiva arrays
    ;;; (primitivaArray ("length") length-primArr)
    ;;; (primitivaArray ("index") index-primArr)
    ;;; (primitivaArray ("slice") slice-primArr)
    ;;; (primitivaArray ("setlist") setlist-primArr)

    ;;Primitiva cadenas
    ;;; (primitivaCadena ("concat") concat-primCad)
    ;;; (primitivaCadena ("string-length") length-primCad)
    ;;; (primitivaCadena ("elementAt") index-primCad)
    
    ;;Variables
    (var-decl ("var" (arbno identificador "=" expresion) "in" expresion) lvar-exp)
    (var-decl ("let" (arbno identificador "=" expresion) "in" expresion) let-exp)
    
    ;;Estructuras de datos
    (struct-decl ("struct" identificador "{" (arbno identificador) "}") struct-exp)

    ;;Expresiones regulares
    ;;; (regular-exp (identificador "::" identificador) list-match-exp)
    ;;; (regular-exp ("numero" "(" identificador ")") num-match-exp)
    ;;; (regular-exp ("cadena" "(" identificador ")") cad-match-exp)
    ;;; (regular-exp ("boolean" "(" identificador ")") bool-match-exp)
    ;;; (regular-exp ("array" "(" (separated-list identificador ",") ")") array-match-exp)
    ;;; (regular-exp ("empty") empty-match-exp)
    ;;; (regular-exp ("default") default-match-exp)
    )
  )

(sllgen:make-define-datatypes lexica gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexica gramatica)))



;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser lexica gramatica))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner lexica gramatica))


;El Interpretador (FrontEnd + Evaluación + señal para lectura +
(define interpretador
  (sllgen:make-rep-loop "--> "
    (lambda (pgm) (eval-program pgm))
    (sllgen:make-stream-parser 
      lexica
      gramatica)))

;El Interprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)
(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (a-programa (structs exp)
                  (elaborate-struct-decls! structs)
                  (eval-expression exp (init-env))))))

;Lista de estructuras
(define the-struct-env '())

;Elaborar structuras declaradas
(define elaborate-struct-decls!
  (lambda (structs)
    (set! the-struct-env structs)))

; Logica para las estructuras

;Ambiente inicial
(define init-env
  (lambda ()
    (extend-env
     '(x y z)
     '(1 2 3)
     (empty-env))))


;Evaluar expresiones
(define eval-expression
  (lambda (exp env)
    (cases expresion exp
      ; Expresiones de la gramática
      (bool-exp (bool-expresion) (eval-bool-expresion bool-expresion env))
      (var-exp (identificador) (apply-env env identificador))
      (num-exp (numero-exp) (eval-num-expresion numero-exp env))

      ;Condicionales
      (if-exp (test-exp true-exp false-exp)
              (if (eval-expression test-exp env)
                  (eval-expression true-exp env)
                  (eval-expression false-exp env)))

      ;Secuenciación y asignación
      (set-exp (id rhs-exp)
               (begin
                 (setref!
                  (apply-env-ref env id)
                  (eval-expression rhs-exp env))
                1))
      (begin-exp (exp exps) 
                 (let loop ((acc (eval-expression exp env))
                             (exps exps))
                    (if (null? exps) 
                        acc
                        (loop (eval-expression (car exps) 
                                               env)
                              (cdr exps)))))

      ;Variables
      (decl-exp (var-decl) (eval-var-decl var-decl env))

      ;Iteradores
      (for-exp (id from until by body)
        (eval-for-exp id from until by body env))
      (while-exp (exp body)
        (eval-while-exp exp body env))

      ;Switch
      (switch-exp (exp cases casesExp default)
        (eval-switch-exp exp cases casesExp default env))

      ;Primitivas
      (prim-num-exp (exp1 primitiva exp2)
        (apply-primitive primitiva (list (eval-expression exp1 env) (eval-expression exp2 env))))
      (prim-bool-exp (primitivaBooleana args) 
        (apply-primitive-bool primitivaBooleana (eval-rands args env)))
    )
  )
)

; funciones auxiliares para evaluar expresiones
;Evaluar expresiones booleanas
(define eval-bool-expresion
  (lambda (exp env)
    (cases bool-expresion exp
      (true-exp () #t)
      (false-exp () #f)
    )
  )
)

;Evaluar expresiones numéricas
(define eval-num-expresion
  (lambda (exp env)
    (cases numero-exp exp
      (decimal-num (digitoDecimal) digitoDecimal)
      (octal-num (digitoOctal) digitoOctal)
      (bin-num (digitoBinario) digitoBinario)
      (hex-num (digitoHexadecimal) digitoHexadecimal)
      (float-num (flotante) flotante)
    )
  )
)

;Evaluar declaraciones de variables
(define eval-var-decl
  (lambda (exp env)
    (cases var-decl exp
      ; ligadura modificable
      (lvar-exp (ids rands body) 
                (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))
      ; ligadura no modificable
      (let-exp (ids rands body) 
                (let ((args (eval-rands rands env)))
                 (eval-expression body
                                  (extend-env ids args env))))
    )
  )
)

;Primitivas
; funcion para aplicar primitivas numericas
(define apply-primitive
  (lambda (prim args)
    (cases primitiva prim
      (sum-prim () (check-base-apply-operation args +))
      (minus-prim () (check-base-apply-operation args -))
      (mult-prim () (check-base-apply-operation args *))
      (mod-prim () (check-base-apply-operation args modulo))
      (elevar-prim () (check-base-apply-operation args expt))
      (menor-prim () (check-base-apply-operation args <))
      (mayor-prim () (check-base-apply-operation args >))
      (menorigual-prim () (check-base-apply-operation args <=))
      (mayorigual-prim () (check-base-apply-operation args >=))
      (diferente-prim () (check-base-apply-operation args (lambda (x y) (not (= x y)))))
      (igual-prim () (check-base-apply-operation args =))
    )
  )
)

;funciones auxiliares para primitivas numericas
;Aplicar operaciones a números con bases
(define check-base-apply-operation
  (lambda (args operation)
    (let
      (
        (num1 (car args))
        (num2 (cadr args))
      )
      (if (and (number? num1) (number? num2))
        (operation num1 num2)
        (if (and (string? num1) (string? num2))
          (letrec
            (
              (lst1 (get-value num1))
              (lst2 (get-value num2))
              (base1 (cadr lst1))
              (base2 (cadr lst2))
            )
            (if (= base1 base2)
              (let
                (
                  (result (operation 
                            (base-x-to-decimal (car lst1) base1) 
                            (base-x-to-decimal (car lst2) base2)))
                )
                (string-append
                  (if (< result 0) "-" "")
                  (get-base-string base1)
                  (decimal-to-base-x (abs result) base1)
                )
              )
              (eopl:error "Error las bases de los números no son iguales")
            )
          )
          (eopl:error "Error no se pueden operar los valores")
        )
      )
    )
  )
)

;Obtener una lista con el valor y la base de un número
(define get-value
  (lambda (num)
    (if (string=? (substring num 0 1) "-")
      (cond
        [(string=? (substring num 1 2) "b") 
          (list (string-append "-" (substring num 2 (string-length num))) 2)]
        [(string=? (substring num 1 3) "0x") 
          (list (string-append "-" (substring num 3 (string-length num))) 8)]
        [(string=? (substring num 1 3) "hx") 
          (list (string-append "-" (substring num 3 (string-length num))) 16)]
      )
      (cond
        [(string=? (substring num 0 1) "b") 
          (list (substring num 1 (string-length num)) 2)]
        [(string=? (substring num 0 2) "0x") 
          (list (substring num 2 (string-length num)) 8)]
        [(string=? (substring num 0 2) "hx") 
          (list (substring num 2 (string-length num)) 16)]
      )
    )
  )
)

;Obtener la cadena base de un número
(define get-base-string
  (lambda (n)
    (cond
      [(= n 2) "b"]
      [(= n 8) "0x"]
      [(= n 16) "hx"]
      [else ""]
    )
  )
)

(define digits "0123456789ABCDEF")

;funcion para obtener una cadena de un numero
(define get-string
  (lambda (n x)
    (cond
      [(< n x) (string (string-ref digits n))]
      [else (string-append
              (get-string (quotient n x) x)
              (string (string-ref digits (remainder n x))))]
    )
  )
)

;funcion para obtener un numero de una cadena
(define get-number
  (lambda (s x)
    (cond
      [(= (string-length s) 0) 0]
      [else (+ (* x (get-number (substring s 0 (- (string-length s) 1)) x))
               (string-index digits (string-ref s (- (string-length s) 1))))]
    )
  )
)

;funcion para obtener el indice de un caracter en una cadena
(define string-index
  (lambda (str char [acc 0])
    (cond
      [(= (string-length str) 0) (eopl:error "No se encuentra el caracter")]
      [(char=? (string-ref str 0) char) acc]
      [else (string-index (substring str 1 (string-length str)) char (+ acc 1))]
    )
  )
)

;funcion para convertir un numero decimal a base x
(define decimal-to-base-x
  (lambda (n x)
    (let
      (
        (sign (if (< n 0) "-" "")) ; si el numero es negativo entonces se pone el signo
        (n (abs n))
      )
      (string-append sign (get-string n x))
    )
  )
)

;funcion para convertir un numero en base x a decimal
(define base-x-to-decimal
  (lambda (s x)
    (let
      (
        (sign (if (string=? (substring s 0 1) "-") -1 1)) ; si el numero es negativo entonces se pone el signo
        (s (substring s (if (string=? (substring s 0 1) "-") 1 0) (string-length s)))
      )
      (* sign (get-number s x))
    )
  )
)

; funcion para aplicar primitivas booleanas
(define apply-primitive-bool
  (lambda (prim args)
    (cases primitivaBooleana prim
      (and-prim () (operation args (lambda (acc x) (and acc x)) #t))
      (or-prim () (operation args (lambda (acc x) (or acc x)) #f))
      (xor-prim () (operation args (lambda (acc x) (xor acc x)) #f))
      (not-prim () (not (car args)))
    )
  )
)

;funciones auxiliares para primitivas booleanos
(define xor
  (lambda (exp1 exp2)
    (cond
      [(and exp1 exp2) #F]
      [exp1 #T]
      [exp2 #T]
      [else #F]
    )
  )
)

;Iteradores
;funcion para evaluar un for
(define eval-for-exp
  (lambda (id from until by body env)
    (letrec
      (
        (valueFrom (eval-expression from env))
        (valueUntil (eval-expression until env))
        (valueBy (eval-expression by env))
        (iterate
          (lambda (i n inc)
            (cond
              [(< i n)
                (begin
                  (eval-expression body (extend-env (list id) (list i) env))
                  (iterate (+ i inc) n inc)
              )]
              [else #t]
            )
          )
        )
      )
      (iterate valueFrom valueUntil valueBy)
    )
  )
)

;funcion para evaluar un while
(define eval-while-exp
  (lambda (exp body env)
    (letrec
      (
        (iterate
          (lambda (value)
            (cond
              [(eval-expression exp env)
                (begin
                  (eval-expression body env)
                  (iterate value)
                )]
              [else #t]
            )
          )
        )
      )
      (iterate exp)
    )
  )
)

;Switch
;funcion para evaluar un switch
(define eval-switch-exp
  (lambda (exp cases casesExp default env)
    (letrec
      (
        (value (eval-expression exp env))
        (lst (map (lambda (x) (eval-expression x env)) cases))
        (lst2 (map (lambda (x) (eval-expression x env)) casesExp))
        (defaultValue (eval-expression default env))
        (iterate
          (lambda (lst lst2 defaultValue)
            (cond
              [(null? lst) defaultValue]
              [(eqv? value (car lst)) (car lst2)]
              [else (iterate (cdr lst) (cdr lst2) defaultValue)]
            )
          )
        )
      )
      (iterate lst lst2 defaultValue)
    )
  )
)

; funciones auxiliares para aplicar eval-expression a cada elemento de una 
; lista de operandos (expresiones)
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

;operation: Aplicar una operación a la lista

(define operation
  (lambda (lst f acc)
    (cond
      [(null? lst) acc]
      [else
        (operation (cdr lst) f (f acc (car lst) ))])))


;true-value?: determina si un valor dado corresponde a un valor booleano falso o verdadero
(define true-value?
  (lambda (x)
    (not (zero? x))))

;Procedimientos
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expresion?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))


;Ambientes

;definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

(define scheme-value? (lambda (v) #t))

;empty-env:      -> enviroment
;función que crea un ambiente vacío
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;extend-env: <list-of symbols> <list-of numbers> enviroment -> enviroment
;función que crea un ambiente extendido
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

;extend-env-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expressions> environment -> environment
;función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body env)))
            (iota len) idss bodies)
          env)))))


;iota: number -> list
;función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
    (deref (apply-env-ref env sym))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))


;Referencias

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

(define deref
  (lambda (ref)
    (primitive-deref ref)))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))

(define setref!
  (lambda (ref val)
    (primitive-setref! ref val)))

(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))

;Funciones Auxiliares

; funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;Interpretador
;(interpretador)

(provide (all-defined-out))
