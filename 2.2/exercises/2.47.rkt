#lang sicp
;; Exercise 2.47: Here are two possible constructors for frames:
;; For each constructor supply the appropriate selectors to produce an implementation for frames.

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (car (cdr frame)))

(define (edge2-frame1 frame)
  (car (cdr (cdr frame))))

(define (edge2-frame2 frame)
  (cdr (cdr frame)))
