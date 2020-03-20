#lang plai

(define-type FAE
  [id (i symbol?)]
  [num (n number?)]
  [add (lhs FAE?) (rhs FAE?)]
  [sub (lhs FAE?) (rhs FAE?)]
  [fun (param symbol?) (body FAE?)]
  [app (fun-expr FAE?) (arg FAE?)])

(define-type Env
  [mtSub]
  [aSub (id symbol?) (value FAE?) (env Env?)])

(define (lookup sub-id env)
  (type-case Env env
    [mtSub () (error 'lookup "Variable libre.")]
    [aSub (id value rest-env)
      (if (symbol=? sub-id id)
        value
        (lookup sub-id rest-env))]))

(define (interp expr env)
  (type-case FAE expr
    [id (i) (lookup i env)]
    [num (n) expr]
    [add (lhs rhs) 
      (num (+ 
        (num-n (interp lhs env)) 
        (num-n (interp rhs env))))]
    [sub (lhs rhs) 
      (num (- 
        (num-n (interp lhs env)) 
        (num-n (interp rhs env))))]
    [fun (param body) expr]
    [app (fun-expr arg)
      (let ([fun-val (interp fun-expr env)])
        (interp 
          (fun-body fun-val) 
          (aSub (fun-param fun-val) (interp arg env) env)))]))

(interp 
  (app (fun 'x (add (id 'x) (id 'y))) (num 2)) 
  (aSub 'y (num 6) (aSub 'y (num 4) (mtSub))))
