;; Define a procedure (tan-cf x k) that computes an approximation to the tangent function based on Lambert’s formula

(define (cont-frac n d k i)
  (if (= k i)
    (/ (n i) (d i))
    (/ (n i) (- (d i) (cont-frac n d k (+ i 1))))))

(define (square n) (* n n))

(define (tan-cf x k)
  (cont-frac (lambda (i)
                (if (= i 1)
                  x
                  (square x)))
             (lambda (i)
                (- (* 2 i) 1)) k 1))
