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

  




  