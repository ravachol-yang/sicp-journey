;;  Define a procedure cont-frac such that evaluating (cont-frac n d k)
;;  computes the value of the k -term finite continued fraction

;; recursive
(define (cont-frac n d k i)
  (if (= k i)
    (/ (n i) (d i))
    (/ (n i) (+ (d i) (cont-frac n d k (+ i 1))))))

;; iterative
(define (cont-frac-iter n d k)
  (define (iter n d k result)
    (if (= k 0)
      result
      (iter n d (- k 1) (/ (n i) (+ (d i) result)))))
  (iter n d (- k 1) (/ (n k) (d k))))
