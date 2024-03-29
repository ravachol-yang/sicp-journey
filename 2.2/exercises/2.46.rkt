#lang sicp
;; Exercise 2.46: A two-dimensional vector v running from the origin to a point
;; can be represented as a pair consisting of an x -coordinate and a y -coordinate.
;; Implement a data abstraction for vectors by giving a constructor make-vect
;; and corresponding selectors xcor-vect and ycor-vect. In terms of your selectors and constructor,
;; implement procedures add-vect, sub-vect, and scale-vect that perform the operations vector addition,
;; vector subtraction, and multiplying a vector by a scalar:

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (2vect v1 v2 op)
  (make-vect (op (xcor-vect v1)
                 (xcor-vect v2))
             (op (ycor-vect v1)
                 (ycor-vect v2))))

(define (add-vect v1 v2)
  (2vect v1 v2 +))

(define (sub-vect v1 v2)
  (2vect v1 v2 -))

(define (scale-vect v scalar)
  (make-vect (* scalar (xcor-vect v))
             (* scalar (ycor-vect v))))

