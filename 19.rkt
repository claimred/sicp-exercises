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
      (car l)
  (cons (list-reverse (cdr l)) (car l))))

  