#lang racket

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

;;; Straightforward 

(define (find-divisor n d)
  (cond ((> (square d) n) n)
        ((divides? d n) d)
        (else (find-divisor n (+ d 1)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (is-prime? n)
  (= n (smallest-divisor n)))

;;; Fermat test

;;; mod(base^exp, m) - ? 
;;; Why can't we just calculate base^exp first using logarithmic algorithm? 
;;; and then divide by m. This way, less remainder calls.
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;;; 1.21

(define (t121) 
  (display "sd 199: ")
  (display (smallest-divisor 199))
  (display "\nsd 1999: ")
  (display (smallest-divisor 1999))
  (display "\nsd 19999: ")
  (display (smallest-divisor 19999)))

