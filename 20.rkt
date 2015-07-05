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

;;; 2.22

(define (square x) (* x x))

(define (list-append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (list-append (cdr l1) l2))))

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              ;; (cons (square (car things)) answer)))) ;; -> (cons first nil) -> (cons second (cons first nil)), reverse order
              ;; (cons answer (square (car things)))))) ;; -> (cons first nil) -> (cons (cons first nil) second), its not a list
              (list-append answer (list (square (car things))))))) ;; correct iterative implementation                     
  (iter items nil))

;;; 2.23

(define (for-each proc items)
  (cond ((null? items) true)
        (else (proc (car items)) (for-each proc (cdr items)) )))
    
(define (test)
  (display (map (lambda (x) (+ x 2)) (list 1 2 3 4 5)))
  (newline)
  (display (square-list1 (list 1 2 3 4 5)))
  (newline)
  (display (square-list2 (list 1 2 3 4 5))))

