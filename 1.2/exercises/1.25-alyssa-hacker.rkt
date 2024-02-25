#lang sicp
;; NO,
(define (expmod base exp m)
        (remainder (fast-expt base exp) m))
;; Slower
