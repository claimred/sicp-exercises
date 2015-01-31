#lang planet neil/sicp

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (cube x) (* x x x))
(define (square x) (* x x))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (close-enough? x y) (> 0.001 (abs (- x y))))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))      
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (average x y)
  (* 0.5 (+ x y)))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt1 x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(define (sqrt2 x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;;; 1.40
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c) ))



