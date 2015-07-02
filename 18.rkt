#lang planet neil/sicp

;;; 2.7

(define (make-interval a b)
  (if (< a b)
      (cons a b)
      (cons b a)))

(define (display-interval i)
  (display "[")
  (display (lower-bound i))
  (display "; ")
  (display (upper-bound i))
  (display "]"))


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

;;; 2.11
;; Three states for each interval
;;         (1)         (2)          (3)
;; -----[------]----[---0---]----[------]----->
;;
;; Hence, 3 * 3 = 9 cases
;; (11, 12, 13,
;;  21, 22, 23,
;;  31, 32, 33)
;; 
(define (mul-interval-smart x y)
  (let ((x1 (lower-bound x)) 
        (y1 (upper-bound x))
        (x2 (lower-bound y))
        (y2 (upper-bound y)))
  (cond ((and (< x1 0) (< y1 0) (< x2 0) (< y2 0)) ;; (11)
         (make-interval (* x1 x2) (* y1 y2)))
        ((and (< x1 0) (< y1 0) (< x2 0) (>= y2 0)) ;; (12)
         (make-interval (* x1 y2) (* x1 x2)))
        ((and (< x1 0) (< y1 0) (>= x2 0) (>= y2 0)) ;; (13)
         (make-interval (* x1 y2) (* y1 x2)))

        ((and (< x1 0) (>= y1 0) (< x2 0) (< y2 0)) ;; (21)
         (make-interval (* y1 x2) (* x1 x2)))

        ((and (< x1 0) (>= y1 0) (< x2 0) (>= y2 0)) ;; (22)
         (make-interval (min (* x1 y2) (* y1 x2))  (max (* y1 y2) (* x1 x2)))) ;; 1 case where 2 muls are not enough
        
        ((and (< x1 0) (>= y1 0) (>= x2 0) (>= y2 0)) ;; (23)
         (make-interval (* x1 y2) (* y1 y2)))

        ((and (>= x1 0) (>= y1 0) (< x2 0) (< y2 0)) ;; (31)
         (make-interval (* y1 x2) (* x1 y2)))
        ((and (>= x1 0) (>= y1 0) (< x2 0) (>= y2 0)) ;; (32)
         (make-interval (* x2 y1) (* y1 y2)))
        ((and (>= x1 0) (>= y1 0) (>= x2 0) (>= y2 0)) ;; (33)
         (make-interval (* x1 x2) (* y1 y2))))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2.0))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2.0))

;;; 2.12
;; e = r(i) / c * 100%
(define (make-center-percent c e )
  (make-center-width c (/ (* e c) 1) ) )

(define (percent i)
  (* 1 (/ (radius-interval i) (center i))))

;;; 2.13
;; Considering only positive intervals
;; e1 ~ 0, e2 ~ 0
;; ] a = (c1, e1), b = (c2, e2) -> [c1 - e1 * c1; c1 + e1 * c1], [c2 - e2 * c2; c2 + e2 * c2]
;; mul(a,b) = [c1 * c2 * (1 - e1) * (1 - e2); c1 * c2 * (1 + e1) * (1 + e2)] =
;; = [c1 * c2 * (1 - e2 - e1 + e1 * e2); c1 * c2 * (1 + e2 + e1 + e1 * e2)] =
;; e1 * e2 ~~ 0
;; = [c1 * c2 - c1 * c2 * (e1 + e2); c1 * c2 + c1 * c2 * (e1 + e2)
;; Hence, e(mul(a * b)) = e(a) + e(b), if e(a) ~ 0 & e(b) ~ 0

(define (unit-test x1 y1 x2 y2)
  (display "\n")
  (display-interval (make-interval x1 y1))
  (display " * ")
  (display-interval (make-interval x2 y2))
  (display "\n")
  (display "Normal mul: ")
  (display-interval (mul-interval (make-interval x1 y1) (make-interval x2 y2)))
  (display "\n")
  (display "\"Smart\" mul: ")
  (display-interval (mul-interval-smart (make-interval x1 y1) (make-interval x2 y2)))
  (display "\n"))
  

(define (test)
  (unit-test -1 -4 -5 -10)
  (unit-test -1 -4 -5 10)
  (unit-test -1 -4 5 10)
  
  (unit-test -1 4 -5 -10)
  (unit-test -1 4 -5 10)
  (unit-test -1 4 5 10)

  (unit-test 1 4 -5 -10)
  (unit-test 1 4 -5 10)
  (unit-test 1 4 5 10))
#|  (unit-test -2 3 2 3)
  (unit-test 2 3 -2 3))|#
  
  