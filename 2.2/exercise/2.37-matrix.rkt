#lang sicp
;; Exercise 2.37:
;; Suppose we represent vectors v = ( v i ) as sequences of numbers,
;; and matrices m = ( m i j ) as sequences of vectors (the rows of the matrix).

(define (dot-product v w)
  (accumulate + 0 (map * v w)))


;; matrix-*-vector
(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))

;; transpose
(define (transpose mat)
  (accumulate-n cons () mat))

;; m*m
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))

;; accumulate
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))
