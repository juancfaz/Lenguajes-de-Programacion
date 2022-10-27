#lang racket

#|

TODO: Poner en ASCII art "LETREC"

SINTAXIS CONCRETA
=================

Program := Expression
Expression := Number
Expression := -(Expression , Expression)
Expression := zero? (Expression)
Expression := if Expression then Expression else Expression
Expression := Identifier
Expression := let Identifier = Expression in Expression
Expression := proc (Identifier) Expression
Expression := (Expression Expression)
Expression := letrec Identifier(Identifier) = Expression in Expression

SINTAXIS ABSTRACTA
==================

Programa:
- (a-program exp1)

Expresión:
- (const-exp num)
- (diff-exp exp1 exp2)
- (zero?-exp exp1)
- (if-exp exp1 exp2 exp3)
- (var-exp var)
- (let-exp var exp1 body)
- (proc-exp var body)
- (call-exp rator rand)
- (letrec-exp p-name b-var p-body letrec-body)

|#

(struct a-program (exp1)
  #:transparent)

(define (program? x)
  (a-program? x))

(struct const-exp (num)
  #:transparent)

(struct diff-exp (exp1 exp2)
  #:transparent)

(struct zero?-exp (exp1)
  #:transparent)

(struct if-exp (exp1 exp2 exp3)
  #:transparent)

(struct var-exp (var)
  #:transparent)

(struct let-exp (var exp1 body)
  #:transparent)

(struct proc-exp (var body)
  #:transparent)

(struct call-exp (rator rand)
  #:transparent)

(struct letrec-exp (p-name b-var p-body letrec-body)
  #:transparent)

(struct begin-exp (exp1 exp2)
  #:transparent)

(define (expression? x)
  (or (const-exp? x)
      (diff-exp? x)
      (zero?-exp? x)
      (if-exp? x)
      (var-exp? x)
      (let-exp? x)
      (proc-exp? x)
      (call-exp? x)
      (letrec-exp? x)
      (begin-exp? x)))

;; parse : S-Expression -> program?
;; Traducción de S-Expressions a sintaxis abstracta de LET
(define (parse x)
  (a-program (parse-expression x)))

;; parse-expression : S-Expression -> expression?
(define (parse-expression x)
  (cond
    [(number? x) (const-exp x)]
    [(symbol? x) (var-exp x)]
    [(pair? x)
     (case (first x)
       [(-) (parse-diff x)]
       [(zero?) (parse-zero? x)]
       [(if) (parse-if x)]
       [(let) (parse-let x)]
       [(procedure) (parse-proc x)]
       [(letrec) (parse-letrec x)]
       [(begin) (parse-begin x)]
       [else
        (if (and (list? x)
                 (= 2 (length x)))
            (parse-call x)
            (error 'parse "expresión no es válida: ~e" x))])]
    [else
     (error 'parse "expresión no es válida: ~e" x)]))

;; parse-diff : pair? -> diff-exp?
;; x es un par de la forma (- . _)
(define (parse-diff x)
  (unless (= (length x) 3)
    (error 'parse "expresión no es válida: ~e" x))
  (diff-exp (parse-expression (second x))
            (parse-expression (third x))))

;; parse-zero? : pair? -> zero?-exp?
;; x es un par de la forma (zero? . _)
(define (parse-zero? x)
  (unless (= (length x) 2)
    (error 'parse "expresión no es válida: ~e" x))
  (zero?-exp (parse-expression (second x))))

;; parse-if : pair? -> if-exp?
;; x es un par de la forma (if . _)
(define (parse-if x)
  (unless (= (length x) 4)
    (error 'parse "expresión no es válida: ~e" x))
  (if-exp (parse-expression (second x))
          (parse-expression (third x))
          (parse-expression (fourth x))))

;; parse-let : pair? -> let-exp?
;; x es un par de la forma (let . _)
(define (parse-let x)
  (unless (= (length x) 3)
    (error 'parse "expresión no es válida: ~e" x))
  (let ([binding (second x)])
    (unless (= (length binding) 2)
      (error 'parse "expresión no es válida: ~e" x))
    (unless (symbol? (first binding))
      (error 'parse "expresión no es válida: ~e" x))
    (let-exp (first binding)
             (parse-expression (second binding))
             (parse-expression (third x)))))

;; parse-proc : pair? -> proc-exp?
;; x es un par de la forma (proc . _)
(define (parse-proc x)
  (unless (= (length x) 3)
    (error 'parse "expresión no es válida: ~e" x))
  (unless (symbol? (second x))
    (error 'parse "expresión no es válida: ~e" x))
  (proc-exp (second x)
            (parse-expression (third x))))

;; parse-call : list? -> call-exp?
;; x es una lista de dos elementos
(define (parse-call x)
  (call-exp (parse-expression (first x))
            (parse-expression (second x))))

(define (parse-letrec x)
  (error 'parse-letrec "no llames al parser por favo': ~e" x))

;; parse-begin : pair? -> begin-exp?
;; x es un par de la forma (begin . _)
(define (parse-begin x)
  (unless (= (length x) 3)
    (error 'parse "expresión no es válida: ~e" x))
  (begin-exp (parse-expression (second x))
             (parse-expression (third x))))

;;;;;;;;;;;;;;
;; ENTORNOS ;;
;;;;;;;;;;;;;;

(define (empty-env)
  null)

(define (apply-env env var)
  (cond [(null? env)
         (error 'environment "variable libre: ~e" var)]
        [(equal? var (first (first env)))
         (if (vector? (second (first env)))
             (vector-ref (second (first env)) 0)
             (second (first env)))]
        [else
         (apply-env (rest env) var)]))

(define (extend-env var val env)
  (cons (list var val) env))

(define (extend-env-rec p-name b-var body saved-env)
  (let ([vec (make-vector 1)])
    (let ((new-env (extend-env p-name vec saved-env)))
      (vector-set! vec 0 (proc-val (procedure b-var body new-env)))
      new-env)))

;;;;;;;;;;;;;;;;;;;;
;; ALMACENAMIENTO ;;
;;;;;;;;;;;;;;;;;;;;

(define empty-store
  (lambda () '()))

(define the-store 'uninitialized)

(define get-store
  (lambda () the-store))

(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

(define reference?
  (lambda (v)
    (integer? v)))

(define newref
  (lambda (val)
    (let ((next-ref (length the-store)))
      (set! the-store (append the-store (list val)))
      next-ref)))

(define deref
  (lambda (ref)
    (list-ref the-store ref)))

(define report-invalid-reference
  (lambda (ref the-store)
    (error 'setref
                "Mala referencia: ~s, en el almacenamiento: ~s"
                ref
                the-store)))

(define setref!
    (lambda (ref val)
      (set! the-store
        (letrec
          ((setref-inner
            (lambda (store1 ref1)
               (cond
                 ((null? store1)
                  (report-invalid-reference ref the-store))
                 ((zero? ref1) (cons val (cdr store1)))
                 (else
                   (cons
                     (car store1)
                     (setref-inner
                      (cdr store1) (- ref1 1)))))))) (setref-inner the-store ref)))))


;;;;;;;;;;;;;;;;;;;;
;; PROCEDIMIENTOS ;;
;;;;;;;;;;;;;;;;;;;;

(struct procedure (var body saved-env)
  #:transparent)

(define (apply-procedure proc val)
  (unless (procedure? proc)
    (error 'value-of "no es un procedimiento: ~e" proc))
  (let ([var (procedure-var proc)]
        [body (procedure-body proc)]
        [saved-env (procedure-saved-env proc)])
    (value-of body (extend-env var val saved-env))))

;;;;;;;;;;;;;;;
;; SEMÁNTICA ;;
;;;;;;;;;;;;;;;

#|

VALORES EXPRESADOS Y DENOTADOS

ExpVal = Int + Bool + Proc
DenVal = Int + Bool + Proc

|#

(struct num-val (num)
  #:transparent
  #:guard (lambda (num type-name)
            (unless (number? num)
              (error type-name "no es un número: ~e" num))
            num))

(define expval->num num-val-num)

(struct bool-val (bool)
  #:transparent
  #:guard (lambda (bool type-name)
            (unless (boolean? bool)
              (error type-name "no es un booleano: ~e" bool))
            bool))

(define expval->bool bool-val-bool)

(struct proc-val (proc)
  #:transparent
  #:guard (lambda (proc type-name)
            (unless (procedure? proc)
              (error type-name "no es un procedimiento: ~e" proc))
            proc))

(define expval->proc proc-val-proc)
#|

ESPECIFICACIONES SEMÁNTICAS

(value-of (const-exp n) env) = (num-val n)

(value-of (var-exp var) env) = (apply-env env var)

(value-of (diff-exp exp1 exp2) env)
 = (num-val
    (- (expval->num (value-of exp1 env))
       (expval->num (value-of exp2 env))))

(value-of (zero?-exp exp1) env)
 = (if (equal? 0 (expval->num (value-of exp1 env)))
       (bool-val #t)
       (bool-val #f))

(value-of (if-exp exp1 exp2 exp3) env)
 = (if (expval->bool (value-of exp1 env))
       (value-of exp2 env)
       (value-of exp3 env))

(value-of (let-exp var exp1 body) env)
 = (value-of body (extend-env var (value-of exp1 env) env))

(value-of (proc-exp var body) env)
 = (proc-val (procedure var body env))

(value-of (call-exp rator rand) env)
 = (let ([proc (expval->proc (value-of rator env))]
         [arg (value-of rand env)])
     (apply-procedure proc arg))

|#

(define (value-of exp env)
  (cond
    [(const-exp? exp)
     (let ([n (const-exp-num exp)])
       (num-val n))]
    [(var-exp? exp)
     (let ([var (var-exp-var exp)])
       (apply-env env var))]
    [(diff-exp? exp)
     (let ([exp1 (diff-exp-exp1 exp)]
           [exp2 (diff-exp-exp2 exp)])
       (num-val
        (- (expval->num (value-of exp1 env))
           (expval->num (value-of exp2 env)))))]
    [(zero?-exp? exp)
     (let ([exp1 (zero?-exp-exp1 exp)])
       (if (equal? 0 (expval->num (value-of exp1 env)))
           (bool-val #t)
           (bool-val #f)))]
    [(if-exp? exp)
     (let ([exp1 (if-exp-exp1 exp)]
           [exp2 (if-exp-exp2 exp)]
           [exp3 (if-exp-exp3 exp)])
       (if (expval->bool (value-of exp1 env))
           (value-of exp2 env)
           (value-of exp3 env)))]
    [(let-exp? exp)
     (let ([var (let-exp-var exp)]
           [exp1 (value-of (let-exp-exp1 exp) env)]
           [body (let-exp-body exp)])
       (value-of body (extend-env var exp1 env)))]
    [(proc-exp? exp)
     (let ([var (proc-exp-var exp)]
           [body (proc-exp-body exp)])
       (proc-val (procedure var body env)))]
    [(call-exp? exp)
     (let ([rator (call-exp-rator exp)]
           [rand (call-exp-rand exp)])
       (let ([proc (expval->proc (value-of rator env))]
             [arg (value-of rand env)])
         (apply-procedure proc arg)))]
    [(letrec-exp? exp)
     (let ([p-name (letrec-exp-p-name exp)]
           [b-var (letrec-exp-b-var exp)]
           [p-body (letrec-exp-p-body exp)]
           [letrec-body (letrec-exp-letrec-body exp)])
       (value-of letrec-body (extend-env-rec p-name b-var p-body env)))]
    [(begin-exp? exp)
     (let ([exp1 (begin-exp-exp1 exp)]
           [exp2 (begin-exp-exp2 exp)])
       (letrec ([value-of-begins (lambda (e1 es)
                                   (let ([v1 (value-of e1 env)])
                                     (if (null? es)
                                         v1
                                         (value-of-begins (car (list es)) (cdr (list es))))))])
         (value-of-begins exp1 exp2)))]
    [else
     (error 'value-of "no es una expresión: ~e" exp)]))

(define (init-env)
  (foldl (lambda (binding env)
           (extend-env (first binding) (second binding) env))
         (empty-env)
         (list (list 'π (num-val 3.141592653589793))
               (list 'e (num-val 2.718281828459045))
               (list 'i (num-val 0+1i))
               (list 'G (num-val 6.674e-11))
               (list 'c (num-val 299792458))
               (list 'h (num-val 6.62607015e-34)))))

(define (value-of-program pgm)
  (if (program? pgm)
      (let ([exp1 (a-program-exp1 pgm)])
        (value-of exp1 (init-env)))
      (error 'value-of-program "no es un programa: ~e" pgm)))

(define (run sexp)
  (value-of-program (parse sexp)))
