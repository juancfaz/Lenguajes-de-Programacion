#lang plait

(print-only-errors #t)

(define-type Value
  (numV [n : Number])
  (strV [s : String])
  (boolV [b : Boolean])
  )

(define-type ExprC
  (numC [n : Number])
  (strC [s : String])
  (idC [name : Symbol])
  (boolC [b : Boolean])
  (plusO [l : ExprC] [r : ExprC])
  (appendO [l : ExprC] [r : ExprC])
  #|(numeqO [l : Number] [r : ExprC])
  (streqO [l : ExprC] [r : ExprC])|#
  )

(define (bad-arg-to-op-error [op : Symbol] [v : Value])
  (error 'interp
         (string-append
          "bad argument to operator "
          (string-append
           (to-string op)
           (string-append
            ": "
            (to-string v))))))

(define (unbound-identifier-error [name : Symbol])
  (error 'interp
         (string-append
          "unbound identifier: "
          (to-string name))))


(define-type Binding
  (binding [name : Symbol]
        [value : Value]))

(define-type-alias Env (Listof Binding))

(define empty-env empty)

(define (extend-env name value env)
  (cons (binding name value) env))

(define (lookup-env name env)
  (if (empty? env)
      (unbound-identifier-error name)
      (if (eq? name (binding-name (first env)))
          (binding-value (first env))
          (lookup-env name (rest env)))))

(define-type ExprS
  (numS [n : Number])
  (strS [s : String])
  (idS [name : Symbol])
  (boolS [b : Boolean])
  (plusS [l : ExprS] [r : ExprS])
  (appendS [l : ExprS] [r : ExprS])
  #|(ifS [a : ExprS] [b : ExprS] [c : ExprS])
  (lamS [param : Symbol]
        [body : ExprS])
  (funS [param : Symbol]
        [body : ExprS])
  (appS [func : ExprS]
         [arg : ExprS])
  (andS [l : ExprS] [r : ExprS])
  (orS [l : ExprS] [r : ExprS])
  (binopS [l : ExprS] [r : ExprS])|#
  )

(define (interp [e : ExprC] [env : Env]) : Value
  (type-case ExprC e
    [(numC n) (numV n)]
    [(strC s) (strV s)]
    [(idC name) (lookup-env name env)]
    [(boolC b) (boolV b)]
    [(plusO l r)
     (let ([v1 (interp l env)]
           [v2 (interp r env)])
       (cond [(not (numV? v1))
              (bad-arg-to-op-error '+ v1)]
             [(not (numV? v2))
              (bad-arg-to-op-error '+ v2)]
             [else
              (numV (+ (numV-n v1) (numV-n v2)))]))]
    [(appendO l r)
     (let ([v1 (interp l env)]
           [v2 (interp r env)])
       (cond [(not (strV? v1))
              (bad-arg-to-op-error '++ v1)]
             [(not (strV? v2))
              (bad-arg-to-op-error '++ v2)]
             [else
              (strV (string-append (strV-s v1) (strV-s v2)))]))]
    ))

(define (desugar [e : ExprS]) : ExprC
  (type-case ExprS e
    [(numS n) (numC n)]
    [(strS s) (strC s)]
    [(idS name) (idC name)]
    [(boolS b) (boolC b)]
    [(plusS l r) (plusO (desugar l) (desugar r))]
    [(appendS l r) (appendO (desugar l) (desugar r))]
    ))

(define (parse-error e)
  (error 'parse (string-append "malformed-input: " (to-string e))))

(define (parse [in : S-Exp]) : ExprS
  (cond
  [(s-exp-number? in) (parse-number in)]
  [(s-exp-string? in) (parse-string in)]
  [(s-exp-match? `true in) (boolS #t)]
  [(s-exp-match? `false in) (boolS #f)]
  ;;[(s-exp-match? `{if ANY ...} in) (parse-if in)]
  ;;[(s-exp-match? `{and ANY ...} in) (parse-and in)]
  ;;[(s-exp-match? `{or ANY ...} in) (parse-or in)]
  #|[(s-exp-match? `{+ ANY ...} in) (parse-+ in)]
  [(s-exp-match? `{++ ANY ...} in) (parse-++ in)]
  [(s-exp-match? `{num= ANY ...} in) (parse-num= in)]
  [(s-exp-match? `{str= ANY ...} in) (parse-str= in)]
  [(s-exp-match? `{fun ANY ...} in) (parse-fun in)]
  [(s-exp-match? `{let {SYMBOL ANY} ANY ...} in) (parse-let in)]
  [(s-exp-match? `{ANY ...} in) (parse-app in)]
  [(s-exp-symbol? in) (parse-id in)]|#
  ))

(define (parse-number in)
  (numS (s-exp->number in)))

(define (parse-string in)
  (strS (s-exp->string in)))

(define (parse-id in)
  (idS (s-exp->symbol in)))

#|(define (parse-if in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 4)
        (ifS (parse (second inlst))
             (parse (third inlst))
             (parse (fourth inlst)))
        (error 'parse "cantidad incorrecta de argumentos para if"))))

(define (parse-and in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (andS (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para and"))))

(define (parse-or in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (orS (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para or"))))

(define (parse-+ in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (plusO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para +"))))

(define (parse-++ in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (appendO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para ++"))))

(define (parse-num= in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (numeqO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para num="))))

(define (parse-str= in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (binopS (streqO) (parse (second inlst)) (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para str="))))

(define (parse-fun in)
  (cond
    [(s-exp-match? `{fun SYMBOL ANY ...} in)
      (let ([inlst (s-exp->list in)])
        (if (equal? (length inlst) 3)
            (funS (s-exp->symbol (second inlst)) (parse (third inlst)))
            (error 'parse "funciones deben tener solo un cuerpo")))]
    [(s-exp-match? `{fun ANY ...} in)
      (error 'parse "parametros a función deben ser símbolos")]))

(define (parse-let in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 3)
        (letS
          (s-exp->symbol (first (s-exp->list (second inlst))))
          (parse (second (s-exp->list (second inlst))))
          (parse (third inlst)))
        (error 'parse "cantidad incorrecta de argumentos para let"))))

(define (parse-app in)
  (let ([inlst (s-exp->list in)])
    (if (equal? (length inlst) 2)
      (appS (parse (first inlst)) (parse (second inlst)))
      (error 'parse "cantidad incorrecta de argumentos en aplicación de funciones"))))
|#
(define (eval [str : S-Exp]) : Value
  (interp (desugar (parse str)) empty-env))