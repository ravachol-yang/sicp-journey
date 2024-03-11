#lang sicp
;; Exercise 2.69: The following procedure takes as its argument a list of symbol-frequency pairs
;; (where no symbol appears in more than one pair) and generates a Huffman encoding tree according to the Huffman algorithm.

;; Make-leaf-set is the procedure given above that transforms the list of pairs into an ordered set of leaves.
;; Successive-merge is the procedure you must write,
;; using make-code-tree to successively merge the smallest-weight elements of the set until there is only one element left,
;; which is the desired Huffman tree. 

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) 
                (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) 
         (cons x set))
        (else 
         (cons (car set)
               (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set 
         (make-leaf (car pair)    ; symbol
                    (cadr pair))  ; frequency
         (make-leaf-set (cdr pairs))))))

(define (successive-merge pairs)
  (if (= (length pairs) 1) 
      (car pairs)
      (successive-merge 
       (adjoin-set (make-code-tree (car pairs) 
                                        (cadr pairs))
                        (cddr pairs)))))

(define (generate-huffman-tree pairs)
  (successive-merge 
   (make-leaf-set pairs)))
