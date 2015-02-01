#lang planet neil/sicp

;;;; 2.2

(define (average x y)
  (/ (+ x y) 2.0))

;;; Segments

(define (make-segment a b)
  (cons a b))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

;;; Points

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))

(define (mid-point s)
  (make-point (average (x-point (start-segment s)) (x-point (end-segment s)))
              (average (y-point (start-segment s)) (y-point (end-segment s)))))

;;;; 2.3

;;; Rectangles 

;;; Lets assume we are talking about rectangles which sides are parallel to the axes

;;; First representation 


#|(define (make-rect a b)
  (cons a b))

(define (a-point r)
  (car r))

(define (b-point r)
  (cdr r))

(define (get-rect-width r)
  (abs (- (x-point (a-point r)) (x-point (b-point r)))))

(define (get-rect-height r)
  (abs (- (y-point (a-point r)) (y-point (b-point r)))))|#


  
;;; These procedures wont work for different rect representations
#|(define (area r) 
  (abs (* (- (x-point (a-point r)) (x-point (b-point r)))
          (- (y-point (a-point r)) (y-point (b-point r))))))

(define (perimeter r)
  (* 2 (+ (abs (- (x-point (a-point r)) (x-point (b-point r))))
          (abs (- (y-point (a-point r)) (y-point (b-point r)))))))|#

;;; Second representation, point and width & height 

(define (make-rect1 a w h)
  (cons a (cons w h)))
  
(define (get-a-point r)
  (car r))
  
(define (get-sizes r)
  (cdr r))
    
(define (get-rect-width r)
  (car (cdr r)))

(define (get-rect-height r)
  (cdr (cdr r)))

;;; These, however, will. All we need is correct corresponding 
;;; get-width and get-height procedures
    
(define (area r) 
  (* (get-rect-width r) (get-rect-height r) ))

(define (perimeter r)
  (* 2 (+ (get-rect-width r) (get-rect-height r))))

(define (mm)
  (let ( (rr (make-rect1 (make-point 1 1) 3 2)) )                       
    (display (area rr))
    (newline)
    (display (perimeter rr))))


;;; Abstraction barriers
;;; ---------------
;;; area, perimeter
;;; ---------------
;;; get-width, get-height
;;; ---------------
;;; rectangulars as points, as point + size