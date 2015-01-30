#lang planet neil/sicp

;;; 1.37

#|(define (cont-frac n d k)
  (define (cont-frac-help j)
    (if (= j k)
        (/ (n k) (d k))
        (/ (n j) (+ (d j) (cont-frac-help (+ j 1))))))    
  (cont-frac-help 1))|#

(define (cont-frac n d k) ;;; 1.37 b)
  (define (cont-frac-iter i result)
    (if (= i 1)
        result
        (cont-frac-iter (- i 1) (/ (n (- i 1)) (+ (d (- i 1)) result)))))  
  (cont-frac-iter k (/ (n k) (d k))))

(define (euler-formula k) ;;; 1.38
  (define (d i)
    (if (= 2 (remainder i 3))
        (* 2 (+ 1 (quotient i 3)))
        1))           
  (+ 2 (cont-frac (lambda (i) 1.0) d k)))

(define (square x)
  (* x x))

(define (tan-cf x k) ;;; 1.39
  
  (define (n i)
    (if (= i 1)
        x
        (- 0 (square x))   ))
  
  (define (d i)
    (- (* i 2) 1))
  
  (cont-frac n d k))
  
  
        

(define (phi) (* 0.5 (+ 1 (sqrt 5))))


(define (mm1 n)
  (define (iter i)
    (cond ((<= i n) 
           (newline)
           (display "k = ") 
           (display i)
           (display ": ")
           (display (cont-frac (lambda (i) 1.0) 
                               (lambda (i) 1.0) i))
           (iter (+ i 1)))))
  (iter 1)
  (newline)
  (display "1 / phi = ")
  (display (/ 1.0 (phi))))