(define (accumulate-filter
 combiner filter null-value term a next b)
  (if (> a b)
    null-value
    (combiner (if (filter a) null-value (term a))
              (accumulate
               combiner filter term (next a) next b))))
