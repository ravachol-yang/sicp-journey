;; start from row = 0 and col = 0
(define (pascal row col)
  (if (or (= col 0) (= row col))
    1
    (+ (pascal (- row 1) (- col 1))
       (pascal (- row 1) col))))
