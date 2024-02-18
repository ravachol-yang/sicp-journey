#lang sicp
(define (iter-multi))

(define (even? n)
  (= (remainder n 2) 0))

(define (double n)
  (* n 2))

(define (halve n)
  (/ n 2))

(define (fast-mult b n)
  (mult-iter b 1 n b))

(define (mult-iter a b n base)
  (cond ((= n 1)
	 (+ a b))
	((even? n)
	 (mult-iter (double a) b (halve n) base))
	(else (mult-iter a (+ b base) (- n 1) base))))
