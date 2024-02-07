#lang sicp
(define (timed-prime-test n)
        (newline)
        (display n)
        (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
        (if (prime? n)
            (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
        (display " *** ")
        (display elapsed-time))

;; prime?

(define (square n)
        (* n n))

(define (smallest-divisor n)
        (find-divisor n 2))

(define (find-divisor n test-divisor)
        (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
        (= (remainder b a) 0))

(define (prime? n)
        (= (smallest-divisor n) n))


;; Search

(define (timed-prime-test n)
        (start-prime-test n (runtime) 0))
(define (start-prime-test n start-time count)
        (if (= count 3)
            (newline)
            ((if (prime? n)
             ((report-prime (- (runtime) start-time) n)
              (start-prime-test (inc n) (runtime) (inc count)))
             (start-prime-test (inc n) (runtime) count)))))

(define (report-prime elapsed-time n)
        (newline)
        (display n)
        (display " *** ")
        (display elapsed-time))

;; prime?

(define (square n)
        (* n n))

(define (smallest-divisor n)
        (find-divisor n 2))

(define (find-divisor n test-divisor)
        (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
        (= (remainder b a) 0))

(define (prime? n)
        (= (smallest-divisor n) n))






