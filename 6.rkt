#lang sicp

(define (square x)
  (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

;;; Straightforward + 1.23

(define (next-divisor n) 
  (if (= n 2) 3
      (+ n 2)))

(define (find-divisor n d)
  (cond ((> (square d) n) n)
        ((divides? d n) d)
        (else (find-divisor n (next-divisor d)))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (is-prime? n)
  (= n (smallest-divisor n)))

;;; Fermat test

;;; mod(base^exp, m) - ? 
;;; Why can't we just calculate base^exp first using logarithmic algorithm? 
;;; and then divide by m. This way, less remainder calls. 
;;;
;;; In fact, exercise number 1.25 gives an answer to this. By powering first
;;; and finding remainder second we would get large intermediate numbers.
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (try-it a n)
    (= (expmod a n n) a)) ;;; Fermat theorem right here

(define (fermat-test n)  
  (try-it (+ 1 (random (- n 1))) n))

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

;;; 1.22

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (report-prime elapsed-time)
  (display " is prime. T: ")
  (display elapsed-time))

(define (start-prime-test n start-time)
  (if (is-prime? n) (report-prime (- (runtime) start-time)) 0))

(define (search-for-primes a b)
  (define (sfp-iter i)
    (cond ((< i b)
           (timed-prime-test i)
           (sfp-iter (+ i 2)))
          (else
           (display "\nEnd"))))
             
  (if (even? a) (sfp-iter (+ a 1))
      (sfp-iter a)))

;;; 1.27, checking Carmichael numbers
;;;
;;; Note: lisp is hard

(define (cm-check n)
  
  (define (cm-check-iter a)
    (cond ( (< a n) (if (try-it a n) (cm-check-iter (+ a 1)) (display "Not a prime number")))
          ( (= a n) (display "Calculation complete")) ))        
                    
  (cm-check-iter 1))
