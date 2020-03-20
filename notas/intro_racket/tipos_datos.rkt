#lang plai

;; Definición de tipos de datos con define-type
;;
;; (define-type <Nombre>
;;   [<nombre-constructor-1 (<parametro1> <tipo>?)*]+)
;; 
;; Nombre -> Notación Pascal. Ej: Lista, WAE, ArbolBinario
;; Un listado de constructores:
;;   - Un listado de parámetros, que puede ser vecío
;;   - Cada parámetro tiene un nombre y un tipo. El tipo se
;;     se especifica con un predicado.

;; Se define el predicado any?
(define (any? x) #t)

(define-type Arbol
  [hoja (elem any?)]
  [nodo (elem any?) (izq Arbol?) (der Arbol?)])

(hoja 17)
(nodo 1 (hoja 2) (hoja 3))

;; Al definir un tipo de dato se definen en automatico funciones para ese tipo.
;;
;; - Predicado para el tipo de dato:
;; Arbol? : any -> boolean
;; 
;; - Predicado para cada constructor:
;; hoja? : any -> boolean
;; nodo? : any -> boolean
;;
;; - Funciones de acceso para los parámetros.
;; hoja-elem : Arbol -> any
;; nodo-elem : Arbol -> any
;; nodo-izq : Arbol -> Arbol
;; nodo-der : Arbol -> Arbol
;; 
;; - Funciones modificadoras para los parámetros:
;; set-hoja-elem! : Arbol -> any -> void
;; set-nodo-elem! : Arbol -> any -> void
;; set-nodo-izq! : Arbol -> Arbol -> void
;; set-nodo-der! : Arbol -> Arbol -> void

(let ([h (hoja 1)])
  (set-hoja-elem! h 3)
  h)

;; Función que devuelve el número de hojas de un Arbol
;; num-hojas: Arbol -> integer
(define (num-hojas arbol)
  (if (hoja? arbol)
    1
    (+ (num-hojas (nodo-izq arbol)) (num-hojas (nodo-der arbol)))))

(test (num-hojas (hoja 1)) 1)
(test (num-hojas (nodo 2 (hoja 1) (hoja 3))) 2)
(test (num-hojas (nodo 2 (nodo 1 (hoja 4) (hoja 5)) (hoja 3))) 3)


;; Función que determina si un elemento dado está contenido en un Arbol.
;; contiene?: any Arbol -> boolean
(define (contiene? e a)
  (cond
    [(hoja? a) (equal? e (hoja-elem a))]
    [(nodo? a) (or (equal? e (nodo-elem a)) 
                 (contiene? e (nodo-izq a)) 
                 (contiene? e (nodo-der a)))]))

(test (contiene? 1 (hoja 2)) #f)
(test (contiene? 3 (nodo 2 (hoja 1) (hoja 3))) #t)
(test (contiene? 4 (nodo 2 (nodo 1 (hoja 3) (hoja 4)) (hoja 3))) #t)

;; Pero es mejor usar caza de estructuras en vez de revisar condiciones.
;; Ya sea con type-case o match
;;
;; Sintaxis de type-case: 
;; (type-case <Tipo> <expresion>
;;   [<nombre-constructor> (<parametro>*) <expresion>+]+
;;   [else <expresion>*]?)
;;
;; En type-case, si no tenemos todos los contructores necesitamos 
;; forsozamente un else.
;;
;; Sintaxis de match:
;; (match <expresion>
;;   [(<nombre-constructor> <parametro>*) <expresion>+]+
;;   [else <expresion>*]?)

;; Definimos num-hojas pero con type-case
;; num-hojas-type-case: Arbol -> integer
(define (num-hojas-type-case a)
  (type-case Arbol a
    [hoja (e) 1]
    [nodo (e i d) (+ (num-hojas-type-case i) (num-hojas-type-case d))]))

(test (num-hojas-type-case (hoja 1)) 1)
(test (num-hojas-type-case (nodo 2 (hoja 1) (hoja 3))) 2)
(test (num-hojas-type-case (nodo 2 (nodo 1 (hoja 4) (hoja 5)) (hoja 3))) 3)

;; Definimos num-hojas pero con match
;; num-hojas-match: Arbol -> integer
(define (num-hojas-match a)
  (match a
    [(hoja e) 1]
    [(nodo e i d) (+ (num-hojas-match i) (num-hojas-match d))]))

(test (num-hojas-match (hoja 1)) 1)
(test (num-hojas-match (nodo 2 (hoja 1) (hoja 3))) 2)
(test (num-hojas-match (nodo 2 (nodo 1 (hoja 4) (hoja 5)) (hoja 3))) 3)

;; Definimos contiene? pero con type-case
;; contiene-type-case?: any Arbol -> boolean
(define (contiene-type-case? e a)
  (type-case Arbol a
    [hoja (x) (equal? e x)]
    [nodo (x i d) (or (equal? e x) 
                    (contiene-type-case? e i) 
                    (contiene-type-case? e d))]))

(test (contiene-type-case? 1 (hoja 2)) #f)
(test (contiene-type-case? 3 (nodo 2 (hoja 1) (hoja 3))) #t)
(test (contiene-type-case? 4 (nodo 2 (nodo 1 (hoja 3) (hoja 4)) (hoja 3))) #t)

;; Definimos contiene? pero con match
;; contiene-match?: any Arbol -> boolean
(define (contiene-match? e a)
  (match a
    [(hoja x) (equal? e x)]
    [(nodo x i d) (or (equal? e x) 
                    (contiene-match? e i)
                    (contiene-match? e d))]))

(test (contiene-match? 1 (hoja 2)) #f)
(test (contiene-match? 3 (nodo 2 (hoja 1) (hoja 3))) #t)
(test (contiene-match? 4 (nodo 2 (nodo 1 (hoja 3) (hoja 4)) (hoja 3))) #t)
