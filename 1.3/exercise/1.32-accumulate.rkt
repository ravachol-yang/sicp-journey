;; accumulate
(define (accumulate
 combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate
               combiner null-value term (next a) next b))))

;; accumulate iter
(define (accumulate-iter
 combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result a))))
  (iter (term a) null-value))
