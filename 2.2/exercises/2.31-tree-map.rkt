#lang sicp
;; Exercise 2.31:
;; Abstract your answer to Exercise 2.30 to produce a procedure tree-map with the property that square-tree could be defined as
;;
;; (define (square-tree tree)
;;   (tree-map square tree))

(define (tree-map proc tree)
  (cond ((null? tree) nil)
        ((not (pair? tree))
          (proc tree))
        (else (cons (tree-map proc (car tree))
                    (tree-map proc (cdr tree))))))

(define (square n) (* n n))

(define tree (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
