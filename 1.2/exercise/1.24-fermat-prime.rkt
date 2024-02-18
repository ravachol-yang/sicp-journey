#lang sicp
;; Search

(define (timed-prime-test n)
        (start-prime-test n (runtime) 0))
(define (start-prime-test n start-time count)
        (if (= count 3)
            (newline)
            ((if (fast-prime? n 1)
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

(define (expmod base exp m)
        (cond ((= exp 0) 1)
              ((even? exp)
              (remainder (square (expmod base (/ exp 2) m))
                          m))
              (else
              (remainder (* base (expmod base (- exp 1) m))
                          m))))

(define (fermat-test n)
      (define (try-it a)
        (= (expmod a n n) a))
      (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
        (cond ((= times 0) true)
              ((fermat-test n) (fast-prime? n (- times 1)))
              (else false)))
