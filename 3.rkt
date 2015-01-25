#|(define (cube x)
  (* x x x))|#

#|(define (curt x)  
  (define (good-enough-cu? guess)
    (< (abs (- (cube guess) x)) 0.001))  
  (define (improve-cu guess)
    (/ (+ (* 2 guess) (/ x (square guess))) 3))  
  (define (cube-iter guess)    
    (if (good-enough-cu? guess)
        guess
        (cube-iter (improve-cu guess x))))  
  (cube-iter 1.0))|#

#lang racket

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (mysqrt x)  
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(define (akkerman x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (akkerman (- x 1)
                 (akkerman x (- y 1))))))