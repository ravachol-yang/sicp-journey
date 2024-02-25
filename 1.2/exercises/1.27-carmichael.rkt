#lang sicp
;; Exercise 1.27. Demonstrate that the Carmichael numbers listed in footnote 47really do fool the Fermat test.
;; That is, write a procedure that takes an integer n
;; and tests whether a n is congruent to a modulo n for every a<n, and try your
;; procedure on the given Carmichael numbers.

(define (expmod base exp m)
        (cond ((= exp 0) 1)
              ((even? exp)
              (remainder (square (expmod base (/ exp 2) m))
                          m))
              (else
              (remainder (* base (expmod base (- exp 1) m))
                          m))))

(define (square n)
        (* n n))

(define (carmichael-number? n)
        (define (try-it n a)
          (cond ((= a 1) #t)
                ((not (= (expmod a n n) a)) #f)
                (else (try-it n (- a 1)))))
        (try-it n (- n 1)))
