#lang planet neil/sicp

(define (list-length items)
  (if (null? items)
      0
      (+ 1 (list-length (cdr items)))))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x)) (count-leaves (cdr x))))))

;;; 2.24

#| (list 1 (list 2 (list 3 4)))

   (mcons 1 (mcons (mcons 2 (mcons (mcons 3 (mcons 4 '())) '())) '()))

   |*|*| -> |*|/|
    |        |
    1       |*|*| -> |*|/|
             |        |
             2       |*|*| -> |*|/|
                      |        |
                      3        4
             
   (list 1 (list 2 (list 3 4)))
             /\
            /  \
           /    \
          1     (list 2 (list 3 4))
                       /\
                      /  \
                     /    \
                    2     (list 3 4)
                              /\
                             /  \
                            /    \
                           3      4
|#

;;; 2.25

(define x1 (list 1 3 (list 5 7) 9))
(define x2 (list (list 7)))
(define x3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(define (test)
  (display (car (cdr (car (cdr (cdr x1))))))
  (newline)
  (display (car (car x2)))
  (newline)
  (display (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr x3))))))))))))))
  
  
;;; 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

#|
 (append x y) -> (list 1 2 3 4 5 6)
 (cons x y) -> (mcons (list 1 2 3) (list 4 5 6))
 (list x y) -> (mcons (list 1 2 3) (mcons (list 4 5 6) '()))
|#

;;; 2.27

(define (list-append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (list-append (cdr l1) l2))))

(define (list-reverse l)
  (if (null? (cdr l))
      (list (car l)) 
      (list-append (list-reverse (cdr l)) (list (car l)))))

(define (deep-reverse l)
  (if (null? (cdr l))
      (if (pair? (car l))
          (list (deep-reverse (car l)))
          (list (car l)))
       (list-append (deep-reverse (cdr l)) (deep-reverse (list (car l))))))
  

