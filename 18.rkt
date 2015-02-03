#lang planet neil/sicp

;;; 2.7

(define (make-interval a b)
  (if (< a b)
      (cons a b)
      (cons b a)))  

(define (lower-bound i)
  (car i))

(define (upper-bound i)
  (cdr i))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (if (or (= 0 (upper-bound y)) (= 0 (lower-bound y))) ;;; 2.10
      (display "Divided by zero")
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y))))))
;;; 2.8

(define (sub-interval x y)
  #|(make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))|#
  (add-interval x (make-interval (- (upper-bound y)) (- (lower-bound y)))))

(define (radius-interval i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

;;; 2.9
;; [x1, y1] [x2, y2]
;; r1 = (y1 - x1) / 2, r2 = (y2 - x2) / 2
;; "+"
;; [x1 + x2, y1 + y2]
;; r = (y1 + y2 - x1 - x2) / 2
;; r = (y1 - x1) / 2 + (y2 - x2) / 2
;; r = r1 + r2
;; "-"
;; [x1 - y2; y1 - x2]
;; r = (y1 - x2 - x1 + y2) / 2
;; r = (y1 - x1) / 2 + (y2 - x2) / 2
;; r = r1 + r2
;; "*"
;; Let's multiply two different intervals with the same radiuses
;; [0, 1] * [5, 10] = [0, 10]
;; r = 5
;; [4, 5] * [0, 5] = [0, 25]
;; r = 12.5
  
  