#lang sicp
;; Exercise 2.40:
;; Define a procedure unique-pairs that, given an integer n ,
;; generates the sequence of pairs ( i , j ) with 1 ≤ j < i ≤ n .
;; Use unique-pairs to simplify the definition of prime-sum-pairs given above.

 (define (unique-pairs n)
   (flatmap (lambda (i)
              (map (lambda (j) (list i j))
                   (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n)))

 (define (filter predicate sequence)
   (cond ((null? sequence) nil)
         ((predicate (car sequence))
          (cons (car sequence)
                (filter predicate (cdr sequence))))
         (else (filter predicate (cdr sequence)))))

 (define (accumulate op initial sequence)
   (if (null? sequence)
       initial
       (op (car sequence)
           (accumulate op initial (cdr sequence)))))

 (define (enumerate-interval low high)
   (if (> low high)
       nil
       (cons low (enumerate-interval (+ low 1) high))))

 (define (flatmap proc seq)
   (accumulate append nil (map proc seq)))

 (define (prime? x)
   (define (test divisor)
     (cond ((> (* divisor divisor) x) true)
           ((= 0 (remainder x divisor)) false)
           (else (test (+ divisor 1)))))
   (test 2))

 (define (prime-sum? pair)
   (prime? (+ (car pair) (cadr pair))))

 (define (make-sum-pair pair)
   (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))
