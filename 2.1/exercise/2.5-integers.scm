;; Exercise 2.5:
;; Show that we can represent pairs of nonnegative integers using only numbers and arithmetic operations
;; if we represent the pair a and b as the integer that is the product 2^a * 3^b .
;; Give the corresponding definitions of the procedures cons, car, and cdr.

(define (cons x y)
  (* (expt 2 x)
     (expt 3 y)))

(define (car z)
  (if (= 0 (remainder z 2))
      (+ 1 (car (/ z 2)))
      0))

(define (cdr z)
  (if (= 0 (remainder z 3))
      (+ 1 (cdr (/ z 3)))
      0))
