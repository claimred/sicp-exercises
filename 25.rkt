#lang racket

(require "common.rkt")
(require "primes.rkt")

;;; 2.40

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;;; 2.41

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k)) (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (sum-triples n s)
  (filter (lambda (i) (= (accumulate + 0 i) s)) (unique-triples n)))

(define (test)
  (display (sum-triples 5 3))
  (newline)
  (display (sum-triples 5 7))
  (newline)
  (display (sum-triples 5 8)))

(define (prime-sum? pair) 
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum       
       (filter prime-sum? (unique-pairs n))))

(define (remove item seq)
  (filter (lambda (x) (not (= x item)))
          seq))

(define (permutations s)
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

;;; 2.42

(define empty-board nil)

(define (check-list y b1 b2 seq)
  (if (null? seq)
      true
      (if (> (accumulate + 0 (map (lambda (i) (if (or (= (cdr i) y) (= b1 (- (cdr i) (car i))) (= b2 (+ (cdr i) (car i))) ) 1 0) ) seq)) 0)
          false
          true)))

(define (safe? k positions)  
  (let ((x (car (car positions))) (y (cdr (car positions))))  
    (let ((b1 (- y x)) (b2 (+ y x)))
      (check-list y b1 b2 (cdr positions)))))

(define (adjoin-position new-row k rest-of-queens)
  (append (list (cons k new-row)) rest-of-queens))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;; http://oeis.org/A000170, for testing

(define (test-queens)
  (map (lambda (i)
         (newline)
         (display "n = ")
         (display i)
         (display "; ")
         (display "number of ways = ")
         (let ( (numb (length (queens i))) )
           (display numb)
           numb)) (enumerate-interval 1 11)))  


;;; 2.43

(define (queens-new board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

;;; Let boardsize = 8 and t(queens 8) = T
#| (1 2 3 4 5 6 7 8) -> 8 calls of (map .. (queen-cols (- k 1)) and another 8 calls in each one of them, so
   t(queens-new 8) = 8 * ( t(map (queen-cols 7)) + 8 * ( t(map (queen-cols 6)) + ... )
   On the other hand,
   t(queens-new 8) = t(map (queen-cols 7)) + t(map (queen-cols 6)) + ...

   Basically, change of nested maps changes linear recursion to tree recursion |#
