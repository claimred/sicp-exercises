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



  