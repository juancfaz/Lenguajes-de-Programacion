#lang plait

;; Eval

(define (eval [input : S-Exp]) : Number
  (interp (desugar (parse input))))

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

(define (interp [a : ArithC]) : Number
  (type-case ArithC a
    [(numC n) n]
    [(plusC l r) (+ (interp l) (interp r))]
    [(minusC l r) (- (interp l) (interp r))]
    [(multC l r) (* (interp l) (interp r))]))

;; Desugar

(define (desugar [a : ArithS]) : ArithC
  (type-case ArithS a
    [(numS n) (numC n)]
    [(plusS l r) (plusC (desugar l) (desugar r))]
    [(minusS l r) (minusC (desugar l) (desugar r))]
    [(multS l r) (multC (desugar l) (desugar r))]))