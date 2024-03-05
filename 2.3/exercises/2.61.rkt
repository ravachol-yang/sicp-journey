#lang sicp
;; Exercise 2.61:
;; Give an implementation of adjoin-set using the ordered representation.
;; By analogy with element-of-set? show how to take advantage
;; of the ordering to produce a procedure that requires on the average
;; about half as many steps as with the unordered representation.

(define (adjoin-set x set)
  (cond ((< x (car set))
         (cons x set))
        ((= x (car set)) set)
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

;; Tests
(adjoin-set 2 '(1 3 4 5 7))


