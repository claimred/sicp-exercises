#lang sicp

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

(define (close-enough? x y) (> 0.00001 (abs (- x y))))

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
  (/ (+ x y) 2.0))

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

(define (inc x)
  (+ x 1))

;;; 1.41
(define (double f)
  (lambda (x) (f (f x))))

(define (dd f)
  ((double double) f))

;;; 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

;;; 1.43

(define (identity x) x)

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

#|(define (repeated f n)
  (define (iter i result)
    (if (= i n)
        result
        (iter (+ i 1) (compose f result))))
  (iter 0 identity)) ;;; (iter 1 f)|#

;;; 1.44

(define (smooth f)
  (lambda (x) (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3)))

(define (n-fold-smooth f n) 
   ((repeated smooth n) f)) 

;;; 1.45

(define (fast-expt b n)
  (define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n) (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))))
  (fast-expt-iter b n 1))

(define (root2 x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (root3 x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(define (root4 x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (cube y))))) 1.0))

(define (root5 x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (fast-expt y 4)))))
               1.0))

(define (root6 x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (fast-expt y 5)))))
               1.0))

(define (root7 x)
  (fixed-point (average-damp (average-damp (lambda (y) (/ x (fast-expt y 6)))))
               1.0))

(define (root-n x n m) ;;; repeat m times, trying ro figure out m = f(n)
  (define (get-func)
    (lambda (y) (/ x (fast-expt y (- n 1)))))    
  (fixed-point ((repeated average-damp m) (get-func))
               1.0))

;;; So, it seems that (root-n x 4-7 2) works fine
;;; However (root-n x 8 2) doesn't work 
;;; (root-n x 8-15 3) works
;;; (root-n x 16 4) works
;;; I guess, m = floor(log(2, n))

(define (nth-root x n)
  (define (get-m)
    (floor (/ (log n) (log 2))))
  (root-n x n (get-m)))
  
  
  
  
    

