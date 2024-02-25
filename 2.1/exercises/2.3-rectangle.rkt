#lang sicp
;; Exercise 2.3:
;; Implement a representation for rectangles in a plane.
;; (Hint: You may want to make use of Exercise 2.2.) In terms of your constructors and selectors,
;; create procedures that compute the perimeter and the area of a given rectangle.
;; Now implement a different representation for rectangles.
;; Can you design your system with suitable abstraction barriers,
;; so that the same perimeter and area procedures will work using either representation?

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (cons x y))

(define (x-point p)
  (car p))

(define (y-point p)
  (cdr p))

(define (make-segment a b)
  (cons a b))

(define (start-segment s)
  (car s))

(define (end-segment s)
  (cdr s))

(define (midpoint-segment s)
  (make-point (/ (+ (x-point (start-segment s))
                    (x-point (end-segment s)))
              2)
              (/ (+ (y-point (start-segment s))
                    (y-point (end-segment s)))
              2)))

(define (make-rectangle a b)
  (cons a b))

(define (perimeter r)
  (* 2 (+ (car r) (cdr r))))

(define (area r)
  (* (car r ) (cdr r)))

(define (make-rectangle-2 a b c d)
  (cons (make-segment a c)
        (make-segment b d)))



