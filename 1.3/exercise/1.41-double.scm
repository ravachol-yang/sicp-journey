;; Define a procedure double that takes a procedure of one argument as argument and returns a procedure that applies the original procedure twice.
;; For example, if inc is a procedure that adds 1 to its argument,
;; then (double inc) should be a procedure that adds 2. What value is returned by
;; (((double (double double)) inc) 5)
(define (double p)
  (lambda (x)
          (p (p x))))

(define (inc n)
  (+ n 1))

(((double (double double)) inc) 5)
