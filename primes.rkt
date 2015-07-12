#lang racket

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

(provide prime?)