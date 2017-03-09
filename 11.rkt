#lang sicp

;;; 1.33

#|(define (filtered-accumulate filter combiner null-value term a next b) ;;; recursive version
  (if (> a b)
      null-value
      (if (filter a)
          (combiner (term a) 
                    (filtered-accumulate filter combiner null-value term (next a) next b))
          (filtered-accumulate filter combiner null-value term (next a) next b))))|#

(define (filtered-accumulate filter combiner null-value term a next b) ;;; iterative version
  (define (iter a result)
    (if (> a b)
        result
        (if (filter a)
            (iter (next a) (combiner (term a) result))
            (iter (next a) result))))
  (iter a null-value))
                      
(define (sum term a next b)
  (define (combiner x y) (+ x y))
  (define (filter x) true)
  (filtered-accumulate filter combiner 0 term a next b))

(define (sum-of-evens a b)
  (define (combiner x y) (+ x y))
  (define (inc x) (+ x 1))
  (define (identity x) x)
  (define (filter x)
    (even? x))
  (filtered-accumulate filter combiner 0 identity a inc b))

(define (sum-of-integers a b)
  (define (inc x) (+ x 1))
  (define (identity x) x)
  (sum identity a inc b))

#| Prime numbers check procedures (Miller-Rabin test) |#

(define (square x)
  (* x x))

(define (smart-check x m)
  (if (and (not (or (= x 1) (= x (- m 1)))) 
           (= (remainder (square x) m) 1))
      0
      (remainder (square x) m) ))
    
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (smart-check (expmod base (/ exp 2) m) m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (try-it a n)
    (= (expmod a (- n 1) n) 1))

(define (miller-rabin-test n)  
  (try-it (+ 1 (random (- n 1))) n))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (if (= n 1)
      true
      (fast-prime? n 4)))

#| 1.33 a) |#

(define (sum-of-prime-squares a b)
  (define (combiner x y) (+ x y))
  (define (inc x) (+ x 1))
  (define (term x) (square x))
  (define (filter x)
    (prime? x))
  (filtered-accumulate filter combiner 0 term a inc b))  

#| 1.33 b) |#

(define (sum-of-rel-primes n)
  (define (combiner x y) (+ x y))
  (define (inc x) (+ x 1))
  (define (term x) x)
  (define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))))
  (define (filter i)
    (= (gcd i n) 1))
  (filtered-accumulate filter combiner 0 term 1 inc n))

  
(define (mm)
  (display (sum-of-evens 1 10)) ;;; 2 + 4 + 6 + 8 + 10 = 30, 1 + 3 + 5 + 7 + 9 = 25
  (newline)
  (display (sum-of-integers 1 10))
  (newline)
  (display (sum-of-prime-squares 1 10)) ;;; 1 + 4 + 9 + 25 + 49 = 88
  (newline)
  (display (sum-of-rel-primes 11))) ;;; 1 + 3 + 7 + 9
  
  