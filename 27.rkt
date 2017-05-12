#lang sicp

(define a 2)

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;; 2.53


  #|(list 'a 'b 'c)
  (list (list 'george))
  (cdr '((x1 x2) (y1 y2)))
  (cadr '((x1 x2) (y1 y2)))
  (pair? (car '(a short list)))
  (memq 'red '((red shoes) (blue socks)))
  (memq 'red '(red shoes blue socks))|#

;; 2.54

(define (my-equal? p1 p2)
  (cond ((and (null? p1) (null? p2)) #t)
        ((or (null? p1) (null? p2)) #f)
        ((and (pair? p1) (pair? p2))
         (and (equal? (car p1) (car p2))
              (equal? (cdr p1) (cdr p2))))
        ((or (pair? p1) (pair? p2)) #f)
        (else (eq? p1 p2))))

;; (my-equal? '(this is a list) '(this (is a) list))

