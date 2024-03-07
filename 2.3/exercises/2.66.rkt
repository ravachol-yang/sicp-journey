#lang sicp
;; Exercise 2.66: Implement the lookup procedure for the case where the set of records is
;; structured as a binary tree, ordered by the numerical values of the keys.

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (key e) (car e))

(define (lookup given-key tree)
  (cond ((null? tree) #f)
        ((= given-key (key (entry tree)))
         (entry tree))
        ((< given-key (key (entry tree)))
         (lookup given-key (left-branch tree)))
        (else
         (lookup given-key (right-branch tree)))))
