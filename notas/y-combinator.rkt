#lang plai

(define (yc y)
  ((lambda (x) (y (lambda (v) ((x x) v)))) 
   (lambda (x) (y (lambda (v) ((x x) v))))))

(let ([foo (yc (lambda (foo)
                 (lambda (n)
                   (if (zero? n)
                       0
                       (+ (* n n) (foo (- n 1)))))))])
  (foo 0))


(define (p n)
  (match n
    [0 3]
    [1 0]
    [2 2]
    [else (+ (p (- n 1)) (p (- n 3)))]))

(define (p-cola n)
  (p-cola-aux n 3 0 2))

(define (p-cola-aux n n-3 n-2 n-1)
  (match n
    [0 n-3]
    [1 n-2]
    [2 n-1]
    [else (p-cola-aux (sub1 n) n-2 n-1 (+ n-1 n-3))]))

(print-only-errors)

(test (p-cola 4) (p 4))
(test (p-cola 5) (p 5))
(test (p-cola 6) (p 6))
(test (p-cola 10) (p 10))

(define (t n)
  (match n
    [2 1]
    [else (let* ([res (map t (range 2 n))]
                 [rev (reverse res)])
            (foldl (lambda (x y r) (+ r (* x y))) 
                   0 
                   res 
                   rev))]))

(define (t/tr n)
  (t/tr-aux n 1 '(1)))

(define (t/tr-aux n acc results)
  (match n
    [2 acc]
    [else (let ([next (foldl (lambda (x y r) (+ r (* x y))) 
                      0 
                      results 
                      (reverse results))])
            (t/tr-aux (sub1 n) next (cons next results)))]))

(test (t/tr 2) (t 2))
(test (t/tr 4) (t 4))
(test (t/tr 5) (t 5))
(test (t/tr 6) (t 6))
(test (t/tr 10) (t 10))
