#lang planet neil/sicp

;;; 2.4

(define (cons1 x y)
  (lambda (m) (m x y)))

(define (car1 z)
  (z (lambda (p q) p)))

(define (cdr1 z)
  (z (lambda (p q) q)))

;;; 2.5

;; a, b > 0, integer
;; 2^a * 3^b

(define (log_n x n)
  (/ (log x) (log n)))

(define (cons2 a b)
  (* (expt 2 a) (expt 3 b)))

(define (car2 c)
  (define (iter res)
    (if (= 0 (remainder res 3))
        (iter (/ res 3))
        (inexact->exact (round (log_n res 2)))))
  (iter c))
        
(define (cdr2 c)
  (define (iter res)
    (if (= 0 (remainder res 2))
        (iter (/ res 2))
        (inexact->exact (round (log_n res 3)))))
  (iter c))

  

;;; 2.6
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) 
    (lambda (x) 
      (f ((n f) x)))))

;; (add-1 zero)
;; (lambda (f) (lambda (x) (f ((zero f) x))))
;; (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))), (lambda (f) (lambda (x) x)) f = identity
;; (lambda (f) (lambda (x) (f x)))

(define one
  (lambda (f) (lambda (x) (f x))))

;; (add-1 one)
;; (lambda (f) (lambda (x) (f ((one f) x))))
;; (lambda (f) (lambda (x) (f (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

;;(define (plus a b))


 (define (church-to-int cn) 
   ((cn (lambda (n) (+ n 1))) 0)) 


  