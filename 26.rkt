#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))


;; 2.44

(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))



(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit-t painter n)
  (let ((quarter (corner-split painter n)))    
    (let ((half (beside (flip-horiz quarter) quarter)))      
       (below (flip-vert half) half))))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))          
    (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  (compose flip-vert flip-horiz) flip-vert)))
    (combine4 (corner-split painter n))))

;; 2.45

(define (split f1 f2)
  (define (split-helper painter n)
    (if (= n 0)
        painter
        (let ((smaller (split-helper painter (- n 1))))
          (f1 painter (f2 smaller smaller)))))
  split-helper)

(define (up-split-new painter n)
  ((split below beside) painter n))

(define (right-split-new painter n)
  ((split beside below) painter n))


;; test
#|(paint (below (right-split einstein 1) (right-split-new einstein 1)))
(paint (beside (up-split einstein 1) (up-split-new einstein 1)))|#

;;;;;;;;;

;; 2.46

(define (make-vect-new x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* (xcor-vect v) s)
             (* (ycor-vect v) s)))

;;(scale-vect (sub-vect (make-vect-new 0 0) (add-vect (make-vect-new 2 3) (make-vect-new 3 2))) 2)

;; 2.47

(define (make-frame-1 origin edge1 edge2)
  #|(list origin edge1 edge2)|#
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame frame)
  #|(car (cdr (cdr frame))))|#
  (cdr (cdr frame)))

#|(origin-frame (make-frame-1 2 3 4))
  (edge1-frame (make-frame-1 2 3 4))
  (edge2-frame (make-frame-1 2 3 4))|#

;;;;;;;;

(define (frame-coord-map-1 frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

;; 2.48

(define (make-segment-1 a b)
  (cons a b))

(define (start-segment-1 s)
  (car s))

(define (end-segment-1 s)
  (cdr s))


;; 2.49

;; outline
#|(paint (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 0))
                                (make-segment (make-vect 1 0) (make-vect 1 1))
                                (make-segment (make-vect 0 1) (make-vect 1 1))
                                (make-segment (make-vect 0 1) (make-vect 0 0)))))|#

;; X
#|(paint (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                                (make-segment (make-vect 1 0) (make-vect 0 1)))))|#

;; Rhomb
#|(paint (segments->painter (list (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
                                (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
                                (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
                                (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))))|#

;; Wave

(define wave
  (segments->painter (list
  (make-segment
   (make-vect 0.006 0.840)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.006 0.635)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.403 0.646))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.354 0.492))
  (make-segment
   (make-vect 0.403 0.646)
   (make-vect 0.348 0.845))
  (make-segment
   (make-vect 0.354 0.492)
   (make-vect 0.249 0.000))
  (make-segment
   (make-vect 0.403 0.000)
   (make-vect 0.502 0.293))
  (make-segment
   (make-vect 0.502 0.293)
   (make-vect 0.602 0.000))
  (make-segment
   (make-vect 0.348 0.845)
   (make-vect 0.403 0.999))
  (make-segment
   (make-vect 0.602 0.999)
   (make-vect 0.652 0.845))
  (make-segment
   (make-vect 0.652 0.845)
   (make-vect 0.602 0.646))
  (make-segment
   (make-vect 0.602 0.646)
   (make-vect 0.751 0.646))
  (make-segment
   (make-vect 0.751 0.646)
   (make-vect 0.999 0.343))
  (make-segment
   (make-vect 0.751 0.000)
   (make-vect 0.597 0.442))
  (make-segment
   (make-vect 0.597 0.442)
   (make-vect 0.999 0.144))
 (make-segment 
   (make-vect 0.395 0.916)
   (make-vect 0.410 0.916))
  (make-segment 
   (make-vect 0.376 0.746)
   (make-vect 0.460 0.790))
  )))

;; 2.50

;; weird
(define (flip-vertical painter)
   ((transform-painter (make-vect 0.0 1.0)
                       (make-vect 1.0 1.0)
                       (make-vect 0.0 0.0))
    painter))

(define (flip-horizontal painter)
   ((transform-painter (make-vect 1.0 0.0)
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 1.0))
    painter))

;; test
#| (paint (below (beside (flip-vertical einstein) (flip-horizontal einstein)) einstein))|#

(define (rot90 painter)
   ((transform-painter (make-vect 1.0 0.0)
                       (make-vect 1.0 1.0)
                       (make-vect 0.0 0.0))
    painter))

(define (rot180 painter)
  (rot90 (rot90 painter)))

(define (rot270 painter)
  (rot90 (rot90 (rot90 painter))))

;; 2.51

(define (below-a painter1 painter2)
   (let ((split-point (make-vect 0.0 0.5)))
   (let ((paint-bottom
        ((transform-painter (make-vect 0.0 0.0)
                            (make-vect 1.0 0.0)
                            split-point)
         painter1))
        (paint-top
        ((transform-painter split-point
                            (make-vect 1.0 0.5)
                            (make-vect 0.0 1.0))
         painter2)))
   (lambda (frame)
     (paint-bottom frame)
     (paint-top frame)))))
 
(define (below-b painter1 painter2)
   (rot90 (beside (rot270 painter1) (rot270 painter2))))

;; test
#|(paint (beside (below-a wave wave) (below-b wave wave)))|#

;; 2.52
;; a) see 2.49

;; b)
(define (corner-split-b painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1)))
            (corner (corner-split-b painter (- n 1))))
        (beside (below painter up)
                (below right corner)))))
;; test
#|(paint (beside (corner-split wave 2) (corner-split-b wave 2)))|#

;; c)  
(define (square-limit-c painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  (compose flip-vert flip-horiz) flip-vert)))
    (combine4 (rot180 (corner-split painter n)))))

;; test
#| (paint (square-limit-c einstein 2))
   (paint (square-limit einstein 2)) |#








