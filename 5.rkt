#lang racket

(define (square x)
  (* x x))
               
#|(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))|#

;;; 1.16

#|(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))))|#
        
(define (fast-expt b n)
  (define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))))
  (fast-expt-iter b n 1))


;;; 1.17

(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

(define (mul a b)
  (cond ((= a 1) b)
        ((even? a) (double (mul (halve a) b)))
        (else (+ b (mul (- a 1) b)))))

;;; 1.18
;;;; first try
;;; c * a * b + d = const, e = 2^i
       
(define (mul2 a b)
  (define (mul-iter a b c d e)
  (cond ((= a 1) (+ e d))
        ((even? a) (mul-iter (halve a) b (double c) d (double e)))
        (else (mul-iter (- a 1) b c (+ d e) e))))
  (mul-iter a b 1 0 b))

;;; 1.18
;;; second try

(define (mul3 a b)  
  (define (mul-iter a b c)
  (cond ((= a 0) c)
        ((even? a) (mul-iter (halve a) (double b) c))
        (else (mul-iter (- a 1) b (+ c b)))))
  (mul-iter a b 0))        
