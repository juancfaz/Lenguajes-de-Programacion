#lang plait

;; Eval

(define (eval [input : S-Exp]) : Value
  (interp (desugar (parse input))))

;; values

(define-type Value
  (numV [n : Number])
  (ifV [e1 : Value]
       [e2 : Value]
       [e3 : Value]))

;; Operators

(define (numV-op [op : (Number Number -> Number)]
                 [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (op (numV-n l) (numV-n r)))]
    [else
     (error 'numV-op "No es un numero")]))

(define (numV-cmp [cmp : (Number Number -> Boolean)]
                 [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (if (cmp (numV-n l) (numV-n r)) 1 0))]
    [else
     (error 'numV-op "No es un booleano")]))

;; Core

(define-type ArithC
  [numC (n : Number)]
  [plusC (l : ArithC) (r : ArithC)]
  [minusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)]
  [ifC   (e1 : ArithC) (e2 : ArithC) (e3 : ArithC)])

;; Sugar

(define-type ArithS
  [numS (n : Number)]
  [plusS (l : ArithS) (r : ArithS)]
  [minusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)]
  [ifS (e1 : ArithS) (e2 : ArithS) (e3 : ArithS)])

;; Parse

(define (parse [s : S-Exp]) : ArithS
  (cond [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (cond
         [(symbol=? '+ (s-exp->symbol (first sl)))
          (plusS (parse (second sl)) (parse (third sl)))]
         [(symbol=? '- (s-exp->symbol (first sl)))
          (minusS (parse (second sl)) (parse (third sl)))]
         [(symbol=? '* (s-exp->symbol (first sl)))
          (multS (parse (second sl)) (parse (third sl)))]
         [else (error 'parse "operación aritmética malformada")]))]
    [else (error 'parse "expresión aritmética malformada")]))

;; Interpreter

(define (interp [a : ArithC]) : Value
  (type-case ArithC a
    [(numC n) (numV n)]
    [(plusC l r) (numV-op + (interp l) (interp r))]
    [(minusC l r) (numV-op + (interp l) (interp r))]
    [(multC l r) (numV-op * (interp l) (interp r))]
    [(ifC e1 e2 e3) (if (= (numV-n  (interp e1)) 0) (interp e3) (interp e2))]))

;; Desugar

(define (desugar [a : ArithS]) : ArithC
  (type-case ArithS a
    [(numS n) (numC n)]
    [(plusS l r) (plusC (desugar l) (desugar r))]
    [(minusS l r) (minusC (desugar l) (desugar r))]
    [(multS l r) (multC (desugar l) (desugar r))]
    [(ifS e1 e2 e3) (ifC (desugar e1) (desugar e2) (desugar e3))]))