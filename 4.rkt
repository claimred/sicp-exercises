;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
#|(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1)) 
                 (fib (- n 2))))))|#

(define (fib-iter a b n)
  (if (= n 0)
      a    
      (fib-iter b (+ a b) (- n 1)) ))
        
(define (fib n)
  (fib-iter 0 1 n))

;;; 1.11

(define (func-iter a b c n)
  (if (< n 3)
      c
      (func-iter b c (+ a b c) (- n 1))))

(define (func n)
  (if (< n 3)
      n
  (func-iter 0 1 2 n)))
  

;;;; 1.12

(define (c i j)
  (if (or (= j 0) (= i j)) 1
  (+ (c (- i 1) (- j 1)) 
     (c (- i 1) j))))

;;;; 1.14

(define (cube x)
  (* x x x))

(define (p x)
  (- (* 3 x) (* 4 (cube x))))

(define (sine angle)
  (if (< (abs angle) 0.01)
      angle
      (p (sine (/ angle 3)))))

