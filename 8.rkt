#lang planet neil/sicp

(define pi 3.1415926535)

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

;;; 1.30

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))
        

(define (sum-of-integers a b)
  (define (inc x) (+ x 1))
  (define (identity x) x)
  (sum-iter identity a inc b))

(define (sum-of-cubes a b)
  (define (inc x) (+ x 1))  
  (sum cube a inc b))

(define (pi-sum a b)
  (define (pi-term x) (/ 1.0 (* x (+ x 2)))) ;;; 1 / (1 * 3) + 1 / (5 * 7) + 1 / (9 * 11) + ... -> pi/8
  (define (pi-next x) (+ x 4))
  (sum pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum-iter f (+ a (/ dx 2)) add-dx b)
     dx))      

;;; 1.29 first try, working fine but
;;; I am not really sure about get-coeff procedure
;;; 
;;; Answer: we can use k, not x

(define (integral-simpson f a b n)
  (define (is-helper h)
    (define (next x) (+ x h)) 
    
    (define (get-k x)
       (truncate (/ (- x a) h)))
    
    ;;; 1 + 4 + 2 +... + 4 + 1
    ;;; is it possible to get coefficients better?
    (define (get-coeff x) 
      (cond ( (or (= x 0) (= x n)) 1)
            ((even? x) 2)
            (else 4)))    
    
    (define (is-term x) (* (get-coeff (get-k x)) (f x)))
                      
    (* (/ h 3) (sum-iter is-term a next b)))
     
  (if (not (even? n)) 
      -1
  (is-helper (/ (- b a) (* 1.0 n)))))

(define (display-test n e)  
  
  (display "Simple (e = ")
  (display e)
  (display "):\n")
  
  (display (integral cube 0 1 e))
  
  (display "\nSimpson method (n = ")
  (display n)
  (display "):\n")
  (display (integral-simpson cube 0 1 n)))

(define (mm)
  (display "Integral x^3 (0, 1)\n\n")
  (display-test 100 0.01)
  (newline)
  (newline)
  (display-test 1000 0.001)
  (newline)
  (newline)
  (display-test 10000 0.0001)
  (newline)
  (newline))
  