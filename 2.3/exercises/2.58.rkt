#lang sicp
;; Exercise 2.58:
;; Suppose we want to modify the differentiation program
;; so that it works with ordinary mathematical notation, in which + and * are infix rather than prefix operators.
;; Since the differentiation program is defined in terms of abstract data,
;; we can modify it to work with different representations of expressions solely by changing
;; the predicates, selectors, and constructors that define the representation of the algebraic expressions
;; on which the differentiator is to operate.

;; Show how to do this in order to differentiate algebraic expressions presented in infix form,
;; such as (x + (3 * (x + (y + 2)))). To simplify the task,
;; assume that + and * always take two arguments and that expressions are fully parenthesized. 

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
       (eq? (cadr e) '+)))

;; Addend of the sum e.
(define (addend e) (car e))

;; Augend of the sum e.
(define (augend e) (caddr e))

;; Construct the sum of a1 and a2.
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

;; Is e a product?
(define (product? e)
  (and (pair? e)
       (eq? (cadr e) '*)))

;; Multiplier of the product e.
(define (multiplier e) (car e))

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
        (else (list m1 '* m2))))

;; Is e a exponentiation?
(define (exponentiation? e)
  (and (pair? e)
       (eq? (cadr e) '^)))

;; Base of the exponentiation e
(define (base e) (car e))

;; Power of the exponentiation e
(define (power e) (caddr e))

;; Construct the exponentiation of b and p
(define (make-exponentiation b p)
  (cond ((or (=number? b 1)
             (=number? p 0)) 1)
        ((=number? p 1) b)
        ((and (number? b) (number? p))
         (expt b p))
        (else (list b '^ p))))

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
(deriv '(x + (3 * (x + (y + 2)))) 'x)

;; TODO:
;; The problem becomes substantially harder if we allow standard algebraic notation,
;; such as (x + 3 * (x + y + 2)), which drops unnecessary parentheses and assumes
;; that multiplication is done before addition.
;; Can you design appropriate predicates, selectors, and constructors for this notation
;; such that our derivative program still works? 
