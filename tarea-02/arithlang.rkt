#lang plait

;; Eval

(define (eval [input : S-Exp]) : Value
  (interp (desugar (parse input))))

;; values

(define-type Value
  (numV [n : Number]))

;; Operator

(define (numV-op [op : (Number Number -> Number)]
                 [l : Value] [r : Value]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (op (numV-n l) (numV-n r)))]
    [else
     (error 'numV-op "No es un numero")]))

;; Core

(define-type ArithC
  [numC (n : Number)]
  [plusC (l : ArithC) (r : ArithC)]
  [minusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)])

;; Sugar

(define-type ArithS
  [numS (n : Number)]
  [plusS (l : ArithS) (r : ArithS)]
  [minusS (l : ArithS) (r : ArithS)]
  [multS (l : ArithS) (r : ArithS)])

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
    [(minusC l r) (numV-op - (interp l) (interp r))]
    [(multC l r) (numV-op * (interp l) (interp r))]))

;; Desugar

(define (desugar [a : ArithS]) : ArithC
  (type-case ArithS a
    [(numS n) (numC n)]
    [(plusS l r) (plusC (desugar l) (desugar r))]
    [(minusS l r) (minusC (desugar l) (desugar r))]
    [(multS l r) (multC (desugar l) (desugar r))]))