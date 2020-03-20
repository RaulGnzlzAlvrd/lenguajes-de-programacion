#lang plai

;; Tenemos que definir un nuevo tipo de dato Type
(define-type Type
  [tnumber]
  [terror]
  [tarrow (izq Type?) (der Type?)])

;; Cambiamos el lenguaje para definir tipos explícitos
(define-type SAST
  [idS (i symbol?)]
  [numS (n number?)]
  [binopS (f procedure?) (izq SAST?) (der SAST?)]
  [withS (id symbol?) (type Type?) (value SAST?) (body SAST?)]
  [recS (id symbol?) (type Type?) (value SAST?) (body SAST?)]
  [if0S (test-expr SAST?) (then-expr SAST?) (else-expr SAST?)]
  [funS (param symbol?) (ptype Type?) (rtype Type?) (body SAST?)]
  [appS (fun-expr SAST?) (arg SAST?)]
  [throwsS (id symbol?)]
  [try/catchS (id symbol?) (type Type?) (value SAST?) (body? SAST?)])

;; Necesitamos de un parse para tipos
(define (tparse sexp)
  (match sexp
    ['number (tnumber)]
    ['error (terror)]
    [(list x xs) (tarrow (tparse x) (tparse (cadr xs)))]))

;; Continuamos con el parse
(define (parse sexp)
  (match sexp
    [(? symbol?) (idS sexp)]
    [(? number?) (numS sexp)]
    [(list 'with (list id ': type value) body)
      (withS id (tparse type) (parse value) (parse body))]
    [(list 'rec (list id ': type value) body)
      (recS id (tparse type) (parse value) (parse body))]
    [(list 'if0 test-expr then-expr else-expr)
      (if0S (parse test-expr) (parse then-expr) (parse else-expr))]
    [(list 'fun (list param ': ptype) ': rtype body)
      (funS param (tparse ptype) (tparse rtype) (parse body))]
    [(list 'throws id)
      (throwsS id)]
    [(list try/catch (list id ': type) body)
      (try/catchS id (tparse type) (parse body))]
    ;; Operaciones y aplicacion queda exacatamente igual
    ))

;; Vamos a hacer la verificación de tipos sobre SAST
;; La verificación de try/catch es la misma que la de with
;; TODO: Implementar el verificador de tipos

;; En desugar vamos a quitar el tipo de las expresiones para que el interp quede igual
(define (desugar exp)
  (match 
    [(idS i) (id i)]
    [(numS n) (num n)]
    ;; Operacion binaria queda igual
    ;; if0 queda igual
    [(withS id _ value body)
      (app (fun id (desugar body)) (desugar value))]
    [(recS id type value body)
      (desugar (withS id type (appS (idS 'Y) (funS id type type value)) body))]
    [(funS param _ _ body)
      (fun param (desugar body))]
    ;; queda igual [(appS fun-expr arg)]
    [(throwsS id) (throws id)]
    [(try/catchS id type value body)
      (try/catch id value body)]))

;; Interp queda igual ya que desugar nos deja un AST como el que hemos estado ocupando
