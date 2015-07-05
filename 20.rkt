#lang planet neil/sicp

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

;;; 2.21

(define (square-list1 items)
  (if (null? items)
      nil
      (cons (* (car items) (car items))
            (square-list1 (cdr items)))))

(define (square-list2 l)
  (map (lambda (x) (* x x)) l))
  

(define (test)
  (display (map (lambda (x) (+ x 2)) (list 1 2 3 4 5)))
  (newline)
  (display (square-list1 (list 1 2 3 4 5)))
  (newline)
  (display (square-list2 (list 1 2 3 4 5))))