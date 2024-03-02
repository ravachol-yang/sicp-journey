#lang sicp
;; Exercise 2.48: A directed line segment in the plane can be represented as a pair of
;; vectors—the vector running from the origin to the start-point of the segment,
;; and the vector running from the origin to the end-point of the segment.
;; Use your vector representation from Exercise 2.46 to define a representation
;; for segments with a constructor make-segment and selectors start-segment and end-segment.

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


(define (make-segment v1 v2)
  (cons v1 v2))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (add-vect (car s)
            (cdr s)))
