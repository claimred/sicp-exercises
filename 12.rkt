#lang planet neil/sicp

(define (func x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x ((lambda (c) (* c c)) a))
       (* y b)
       (* a b))))

(define (average x y) (* 0.5 (+ x y)))

(define (close-enough? x y) (> 0.001 (abs (- x y))))

(define (search f neg-point pos-point) 
  (let ((mid-point (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        mid-point
        (let ((test-value (f mid-point)))
          (cond ((positive? test-value)
                 (search f neg-point mid-point))
                ((negative? test-value)
                 (search f mid-point pos-point))
                (else mid-point))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Same signs." a b)))))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (display next)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess)
  (newline))

(define (sqrt1 x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

;;; 1.36

(define (mm1)
  (fixed-point (lambda (y) (/ (log 1000) (log y)) ) 2.0)) 

(define (mm2)
  (fixed-point (lambda (y) (average y (/ (log 1000) (log y))) ) 2.0)) 
  
