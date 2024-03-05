#lang sicp
;; Exercise 2.63:
;; Each of the following two procedures converts a binary tree to a list.

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append 
       (tree->list-1 
        (left-branch tree))
       (cons (entry tree)
             (tree->list-1 
              (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list 
         (left-branch tree)
         (cons (entry tree)
               (copy-to-list 
                (right-branch tree)
                result-list)))))
  (copy-to-list tree '()))

;; Figure 2.16
(define tree1
  '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))

(define tree2
  '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))

(define tree3
  '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

;; Tests
(tree->list-1 tree1)
(tree->list-1 tree2)
(tree->list-1 tree3)

(newline)

(tree->list-2 tree1)
(tree->list-2 tree2)
(tree->list-2 tree3)

;; Q1:
;; Do the two procedures produce the same result for every tree?
;; If not, how do the results differ? What lists do the two procedures produce for the trees in Figure 2.16?

;; :I think, yes.

;; Q2:
;; Do the two procedures have the same order of growth in the number of steps
;; required to convert a balanced tree with n elements to a list? If not, which one grows more slowly? 

;; :the second one is O(n);  for the first one, it consists of an O(logn) operation on the tree and a linear append O(n)
;; So it's O(n*logn)
