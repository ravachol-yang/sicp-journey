#lang sicp
;; Exercise 2.51: Define the below operation for painters.
;; Below takes two painters as arguments.
;; The resulting painter, given a frame, draws with the first painter in the bottom
;; of the frame and with the second painter in the top.
;; Define below in two different ways—
;; first by writing a procedure that is analogous to the beside procedure given above,
;; and again in terms of beside and suitable rotation operations (from Exercise 2.50).

(#%require sicp-pict)

;; by writing a procedure that is analogous to the beside procedure
(define (below painter1 painter2)
  (let ((splite-point (make-vect 0.0 0.5)))
    (let ((paint-up (transform-painter
                     painter1
                     splite-point
                     (make-vect 1.0 0.5)
                     (make-vect 0.0 1.0)))
          (paint-down (transform-painter
                       painter2
                       (make-vect 0.0 0.0)
                       (make-vect 1.0 0.0)
                       splite-point)))
      (lambda (frame)
        (paint-up frame)
        (paint-down frame)))))

;; in terms of beside and suitable rotation operations (from Exercise 2.50)
(define (below2 painter1 painter2)
  (rotate90 (beside (rotate270 painter1)
                    (rotate270 painter2))))
