;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

;;; Standard

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (mysqrt x)
  (sqrt-iter 1.0 x))

;;; Modified

(define (good-enough1? guess old-guess)
  (< (abs (- guess old-guess)) 0.001))

(define (sqrt-iter1 guess x old-guess)
  (if (good-enough1? guess old-guess)
      guess
      (sqrt-iter1 (improve guess x) x guess)))

(define (mysqrt1 x)
  (sqrt-iter1 1.0 x x))


;;; Cube root

(define (cube x)
  (* x x x))

(define (good-enough-cu? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (improve-cu guess x)
  (/ (+ (* 2 guess) (/ x (square guess))) 3))

(define (cube-iter guess x)
  (if (good-enough-cu? guess x)
      guess
      (cube-iter (improve-cu guess x) x)))

(define (curt x)
  (cube-iter 1.0 x))