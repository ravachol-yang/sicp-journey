#lang sicp
;;  In this fraction, the N i are all 1, and the D i are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ….

(define (cont-frac n d k i)
  (if (= k i)
    (/ (n i) (d i))
    (/ (n i) (+ (d i) (cont-frac n d k (+ i 1))))))

(cont-frac (lambda (i) 1.0)
           (lambda (i)
            (cond ((= i 2) 2)
                  ((and (> i 2) (= (remainder i 3) 2))
                    (* (+ (/ (- i 2) 3) 1) 2))
                  (else 1)))
           1000
           1)
