#lang sicp
(define (fast-expt-iter a b n)
  (cond ((= n 0)
        a)
        ((even? n)
        (fast-expt-iter a (* b b) (/ n 2)))
        (else
        (fast-expt-iter (* a b) b (- n 1)))))


(define (even? n)
  (= (remainder n 2) 0))
