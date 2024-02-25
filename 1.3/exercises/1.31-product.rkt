#lang sicp
;; product
(define (product term a next b)
  (if (> a b)
    1
    (* (term a)
       (product term (next a) next b))))

;; walli product pie
(define (pi up)
  (define (inc n) (+ n 1))
  (define (term n)
    (* (/ (* 2 n) (- (* 2 n) 1))
       (/ (* 2 n) (+ (* 2 n) 1))))
  (* 2
     (product term 1 inc up)))

;; factorial
(define (factorial b)
  (define (id n) n)
  (define (inc n) (+ n 1))
  (product id 1 inc b))

;; product iter
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* result a))))
  (iter (term a) 1))
