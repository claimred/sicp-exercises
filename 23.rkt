#lang planet neil/sicp

;; enumerator, filter, map, accumulator

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumulate op init (cdr seq)))))

(define (filter predicate seq)
  (cond ((null? seq) nil)
        ((predicate (car seq))
         (cons (car seq)
               (filter predicate (cdr seq))))
         (else (filter predicate (cdr seq)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree) 
  (accumulate + 0 (map (lambda (x) (* x x)) (filter odd? (enumerate-tree tree)))))

;;; 2.33

(define (new-map proc seq)
  (accumulate (lambda (x y) (cons (proc x) y)) nil seq))

(define (new-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (new-length seq)
  (accumulate (lambda (x y) (+ 1 y)) 0 seq))

;;; 2.34

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)) )
              0
              coefficient-sequence))

;;; 2.35

(define (count-leaves tree)
  (accumulate (lambda (x y) (+ 1 y)) 0 (map (lambda (x) x) (enumerate-tree tree))))

;; Without enumerate-tree, using recursion
#|(define (count-leaves tree)
  (accumulate + 0 (map (lambda (x)
                         (if (not (pair? x))
                             1
                             (count-leaves x))) tree)))|#

;;; 2.36

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map (lambda (x) (car x)) seqs))
            (accumulate-n op init (map (lambda (x) (cdr x)) seqs)))))

;; (accumulate-n + 0 (list (list 1 2) (list 4 5)))