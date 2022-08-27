#lang plait

(define (eval [input : S-Exp]) : Value
  (interp (desugar (parse input))))

;; Types

(define-type Value
  (numV [value : Number])
  (boolV [value : Boolean])
  (opV [op : Operator]
        [l : Expr]
        [r : Expr])
  (ifV [e1 : Expr]
       [e2 : Expr]
       [e3 : Expr]))

;; Core

(define-type ExprC
  [numC (n : Number)]
  [plusC (l : ExprC) (r : ExprC)]
  [multC (l : ExprC) (r : ExprC)]
  [minusC (l : ExprC) (r : ExprC)]
  [ifC   (e1 : ExprC) (e2 : ExprC) (e3 : ExprC)])

;; Sugar

(define-type sugar
  [numS (n : value)]
  [plusS (l : sugar) (r : sugar)]
  [multS (l : sugar) (r : sugar)]
  [ifS (e1 : sugar)
       (e2 : sugar)
       (e3 : sugar)])

;; parse

(define (parse [s : S-Exp]) : ArithC
  (cond [(s-exp-number? s) (numC (s-exp->number s))]
        [(s-exp-list? s)
         (let ([ls (s-exp->list s)])
           (cond
         [(symbol=? '+ (s-exp->symbol (first ls)))
          (plusC (parse (second ls)) (parse (third ls)))]
         [(symbol=? '* (s-exp->symbol (first ls)))
          (multC (parse (second ls)) (parse (third ls)))]
         [(symbol=? 'ifC (s-exp->symbol (first ls)))
          (ifC (parse (second ls)) (parse (third ls)) (parse (fourth ls)))]))]))

;; interp

(define (interp [arg : ExprC]) : Value
  (type-case ExprC arg
    [numC (n) (numV n)]
    [plusC (l r) (opV + (interp l) (interp r))]
    [multC (l r) (opV * (interp l) (interp r))]
    [ifC (e1 e2 e3) (if (= (numV-n (interp e1)) 0) (interp e3) (interp e2))]))

;; desugar

(define (desugar [sugar : ArithC]) : ExprC
  (type-case ArithC sugar
    [numS (n) (numC n)]
    [plusS (l r) (plusC (desugar l) (desugar r))]
    [multS (l r) (multC (desugar l) (desugar r))]
    [ifS (e1 e2 e3) (ifC (desugar e1) (desugar e2) (desugar e3))]))