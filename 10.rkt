#lang sicp

(define (cube x) (* x x x))

;;; 1.32

#|(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) 
                (accumulate combiner null-value term (next a) next b))))|#

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result ))))
  (iter a null-value))
  
(define (sum term a next b)
  (define (combiner x y) (+ x y))
  (accumulate combiner 0 term a next b))

(define (product term a next b)
  (define (combiner x y) (* x y))
  (accumulate combiner 1 term a next b))

(define (factorial n)
  (define (identity x) x)
  (define (inc x) (+ x 1))    
  (product identity 1 inc n))

(define (sum-of-integers a b)
  (define (inc x) (+ x 1))
  (define (identity x) x)
  (sum identity a inc b))

(define (sum-of-cubes a b)
  (define (inc x) (+ x 1))  
  (sum cube a inc b))

(define (pi-sum a b)
  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (sum pi-term a pi-next b))

(define (mm)
  (display (sum-of-integers 1 10))
  (newline)
  (display (* 8 (pi-sum 1 1000)))
  (newline)
  (display (factorial 5)))