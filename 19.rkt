#lang planet neil/sicp

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (list-length-rec l)
  (if (null? l)
      0
      (+ 1 (list-length-rec (cdr l)))))

(define (list-length-ite l)
  (define (length-iter a c)
    (if (null? a)
        c
        (length-iter (cdr a) (+ 1 c))))    
  (length-iter l 0))

(define (list-append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (list-append (cdr l1) l2))))

;;; 2.17

(define (last-pair l)
  (if (null? (cdr l))
      (list (car l))
      (last-pair (cdr l))))

;;; 2.18

(define (list-reverse l)
  (if (null? (cdr l))
      (list (car l)) 
  (list-append (list-reverse (cdr l)) (list (car l)))))

(define (test)
  (list-append (list 1 2 3 4 5) (list-reverse (list 1 2 3 4 5))))


;;; 2.19

(define (no-more? l)
  (null? l))

(define (except-first-denomination l)
  (cdr l))

(define (first-denomination l)
  (car l))

;; The order of the coin-values elements doesn't matter
;; because
(define us-coins (list 25 50 10 1 5))
(define uk-coins (list 50 100 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

;;; 2.20

(define (same-parity x . z)
  (define (sp-iter l f)
    (if (null? l)
        nil
        (if (f (car l))
            (cons (car l) (sp-iter (cdr l) f))
            (sp-iter (cdr l) f))))
  (sp-iter z
           (lambda (y) (if (even? x)
                           (even? y)
                           (not (even? y))))))
  
(same-parity 2 3 4 5 6 8 10 11 13 16 17 17 19 20)
(same-parity 1 2 3 4 5 6 8 10 11 13 16 17 17 19 20)


  