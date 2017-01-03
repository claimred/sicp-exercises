#lang sicp

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


;;; 2.38

(define (fold-left op init seq)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init seq))

(define (fold-right op init seq)
  (accumulate op init seq))

;; (fold-right / 1 (list 1 2 3)) = (/ 1 (/ 2 (/ 3 1))) = 3/2
;; (fold-left / 1 (list 1 2 3)) = (/ (/ (/ 1 1) 2) 3) = 1/6
;; (fold-right list nil (list 1 2 3)) = (1 (2 (3 ())))
;; (fold-left list nil (list 1 2 3)) = (((() 1) 2) 3)
;; op must be commutative (and associative?)

;;; 2.39

(define (reverse1 seq)
  (fold-right (lambda (x y) (append y (list x))) nil seq))

(define (reverse2 seq)
  (fold-left (lambda (x y) (cons y x)) nil seq))

(define (test2)
  (fold-right / 1 (list 1 2 3))
  (fold-left / 1 (list 1 2 3)))