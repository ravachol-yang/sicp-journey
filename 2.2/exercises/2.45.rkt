#lang sicp 
;; Exercise 2.45: Right-split and up-split can be expressed as instances of a general splitting operation.
;; Define a procedure split with the property that evaluating:

(#%require sicp-pict)

(define (split one another)
  (lambda (painter)
    (one painter
         (another painter painter))))

(define right-split (split beside below))
(define up-split (split below beside))
