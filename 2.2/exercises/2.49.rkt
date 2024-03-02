#lang sicp
;; Exercise 2.49: Use segments->painter to define the following primitive painters:

;;    The painter that draws the outline of the designated frame.
;;    The painter that draws an “X” by connecting opposite corners of the frame.
;;    The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.
;;    The wave painter. 


(#%require sicp-pict)

(define outline
  (segments->painter (list
                      (make-segment (make-vect 0.0 0.0) (make-vect 0.0 1.0))
                      (make-segment (make-vect 0.0 1.0) (make-vect 1.0 1.0))
                      (make-segment (make-vect 1.0 1.0) (make-vect 1.0 0.0))
                      (make-segment (make-vect 1.0 0.0) (make-vect 0.0 0.0)))))

(define x
  (segments->painter (list
                      (make-segment (make-vect 0.0 0.0) (make-vect 1.0 1.0))
                      (make-segment (make-vect 0.0 1.0) (make-vect 1.0 0.0)))))

(define diamond
  (segments->painter (list
                      (make-segment (make-vect 0.0 0.5) (make-vect 0.5 1.0))
                      (make-segment (make-vect 0.5 1.0) (make-vect 1.0 0.5))
                      (make-segment (make-vect 1.0 0.5) (make-vect 0.5 0.0))
                      (make-segment (make-vect 0.5 0.0) (make-vect 0.0 0.5)))))
