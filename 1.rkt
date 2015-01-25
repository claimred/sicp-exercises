;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |123|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ())))
(define (square x)
  (* x x))

(define (sum-of-squares a b)
  (+ (square a) (square b)))

(define (mymax a b)
  (if (> a b) a b))

(define (mymin a b)
  (if (< a b) a b))

(define (test13 a b c)
  (if (> a b) 
      (sum-of-squares a (mymax b c))
      (sum-of-squares b (mymax a c))))

(define (p1 a b)
  ((if (> b 0) + -) a b))

(define (p) (p))

(define (test15 x y)
  (if (= x 0) 0 y))


   

