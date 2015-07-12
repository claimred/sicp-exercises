#lang planet neil/sicp

;;(map (lambda (x) (+ x 1)) (list 1 2 3 4))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

;;; 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (x) (dot-product x v))  m))

(define (transpose m)
  (accumulate-n cons nil m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (x) (matrix-*-vector cols x))  m)))

(define matr1 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))
(define vect1 (list 1 2 3))

(define (test)
  (transpose matr1))
  ;;(matrix-*-vector matr1 vect1))

