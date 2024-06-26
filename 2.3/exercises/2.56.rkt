#lang sicp
;; Exercise 2.56: Show how to extend the basic differentiator to handle more kinds of expressions.
;; For instance, implement the differentiation rule 
;; by adding a new clause to the deriv program and defining appropriate procedures
;; exponentiation?, base, exponent, and make-exponentiation. (You may use the symbol ** to denote exponentiation.)
;; Build in the rules that anything raised to the power 0 is 1 and anything raised to the power 1 is the thing itself. 

;; is number equal?
(define (=number? exp num)
  (and (number? exp) (= exp num)))

;; Is e a variable?
(define (variable? e) (symbol? e))

;; Are v1 and v2 the same variable?
(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

;; Is e a sum?
(define (sum? e)
  (and (pair? e)
       (eq? (car e) '+)))

;; Addend of the sum e.
(define (addend e) (cadr e))

;; Augend of the sum e.
(define (augend e) (caddr e))

;; Construct the sum of a1 and a2.
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

;; Is e a product?
(define (product? e)
  (and (pair? e)
       (eq? (car e) '*)))

;; Multiplier of the product e.
(define (multiplier e) (cadr e))

;; Multiplicand of the product e.
(define (multiplicand e) (caddr e))

;; Construct the product of m1 and m2.
(define (make-product m1 m2)
  (cond ((or (=number? m1 0)
             (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
         (* m1 m2))
        (else (list '* m1 m2))))

;; Is e a exponentiation?
(define (exponentiation? e)
  (and (pair? e)
       (eq? (car e) '^)))

;; Base of the exponentiation e
(define (base e) (cadr e))

;; Power of the exponentiation e
(define (power e) (caddr e))

;; Construct the exponentiation of b and p
(define (make-exponentiation b p)
  (cond ((or (=number? b 1)
             (=number? p 0)) 1)
        ((=number? p 1) b)
        ((and (number? b) (number? p))
         (expt b p))
        (else (list '^ b p))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (eq? exp var) 1 0))
        ((sum? exp) ;; sum
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp) ;; prodcut
         (make-sum
          (make-product
           (multiplier exp)
           (deriv (multiplicand exp) var))
          (make-product
           (multiplicand exp)
           (deriv (multiplier exp) var))))
        ((exponentiation? exp) ;; exponentiation
         (make-product (power exp)
                       (make-product
                        (make-exponentiation
                         (base exp)
                         (- (power exp) 1))
                        (deriv (base exp) var))))
        (else (error "ERROR" exp))))


;; Tests
(deriv '(+ x 3) 'x) ;; x + 3 -> 1
(deriv '(* x y) 'x) ;; x * y -> y
(deriv '(* (* x y) (+ x 3)) 'x) ;; (x * y) * (x + 3) -> (x * y) + ((x + 3) * y)
(deriv '(^ x 3) 'x) ;; x^3 -> 3 * (x^2)
