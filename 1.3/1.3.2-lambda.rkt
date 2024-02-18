#lang sicp
;; sum
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; Then our pi-sum procedure can be expressed without defining any auxiliary procedures as
(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

;; Again using lambda, we can write the integral procedure without having to define the auxiliary procedure add-dx:
(define (integral f a b dx)
  (* (sum f (+ a (/ dx 2.0))
            (lambda (x) (+ x dx))
            b)
     dx))

;; > 12
((lambda (x y z) (+ x y (square z))) 1 2 3)

;; f ( x , y ) = x ( 1 + x y )^ 2 + y ( 1 − y ) + ( 1 + x y ) ( 1 − y ) ,
(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))

;; with lambda
(define (f x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)))

;; with let
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;; x=5, expression is 38
(+ (let ((x 3))
     (+ x (* x 10)))
   x)

;; x=2, it is 12
(let ((x 3)
      (y (+ x 2)))
  (* x y))
