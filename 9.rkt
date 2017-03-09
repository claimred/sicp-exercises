#lang sicp

(define (square x) (* x x))

;;; 1.31

(define (product term a next b) ;;; 1.31 a)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))

(define (product-iter term a next b) ;;; 1.31 b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))
         
(define (factorial n)
  (define (identity x) x)
  (define (inc x) (+ x 1))    
  (product-iter identity 1 inc n))

(define (calc-pi n) ;;; 1.31 a)
  (define (term x)   
    (* (/ x (- x 1)) (/ x (+ x 1))))
  (define (next x) (+ x 2))
  (product-iter term 2 next n))

(define (mm1)
  (display (factorial 1))
  (newline)
  (display (factorial 3))
  (newline)
  (display (factorial 5))
  (newline)
  (display (factorial 7))
  (newline)
  (display (factorial 10)))

(define (mm2)
  (* 2.0 (calc-pi 5000)))
