#lang sicp
;; Exercise 2.42:
;; The “eight-queens puzzle” asks how to place eight queens on a chessboard
;; so that no queen is in check from any other (i.e., no two queens are in the same row, column, or diagonal).

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0) ;; end at k = 0
        (list empty-board) ;; end with an empty board
        (filter ;; if it isn't the last col (k not 0), filter the rows
         (lambda (positions)
           (safe? k positions)) ;; check if this queen is safe
         (flatmap
          (lambda (rest-of-queens) ;; the rest of the queen already on the board
            (map (lambda (new-row)
                   (adjoin-position
                    new-row
                    k
                    rest-of-queens))
                 (enumerate-interval
                  1
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size)) ;; Start from n

(define (safe? k positions)
  (null? (filter (lambda (p)
                    (+))
                 positions)))

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

;; The empty board is nil
(define empty-board nil)

;; required
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1)
                    list2))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate
                       (cdr sequence))))
        (else  (filter predicate
                       (cdr sequence)))))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low
            (enumerate-interval
             (+ low 1)
             high))))
