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
  
  
  
