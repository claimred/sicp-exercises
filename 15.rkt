#lang sicp

(define (gcd a b)
  (if (= b 0)
      a
  (abs (gcd b (remainder a b)))))

;;; 2.1

(define (make-rat n d)
  (if (< d 0)
      (make-rat (- 0 n) (- 0 d))
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g)))))
                       
(define (numer x) (car x))

(define (denom x) (cdr x))


(define (print-rat x)  
  (display (numer x))
  (display "/")
  (display (denom x))
  (newline))
  
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x)) )
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x)) )
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))

(define (equals? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define r1 (make-rat 1 -2))
(define r2 (make-rat -3 4))

(define (mm)
  (print-rat (make-rat -1 2))
  (print-rat (make-rat 1 -2))
  (print-rat (make-rat -1 -2))
  (print-rat (make-rat 1 2))
  (print-rat (mul-rat r1 r2)))
  #|(print-rat (add-rat (make-rat 3 2) (make-rat 3 4)))
  (print-rat (sub-rat (make-rat 3 2) (make-rat 3 4)))
  (print-rat (mul-rat (make-rat 3 2) (make-rat 3 4)))
  (print-rat (div-rat (make-rat 3 2) (make-rat 3 4))))|#
            





