(define (gcd a b)
  (if (= b 0)
  a
  (gcd b (remainder a b))))

;; (gcd 206 40)

;; Applicative Order Eval
;; (gcd 206 40) = (gcd 40 6)
;;              = (gcd 6 4)
;;              = (gcd 4 2)
;;              = (gcd 2 0)
;;              = 2

;; Normal Order Eval
(gcd 206 40)

(if (= 40 0)
    40
    (gcd 40 (remainder 260 40)))

(if (= (remainder 260 40) 0)
    (remainder 260 40)
    (gcd (remainder 206 40)(remainder 40 (remainder 260 40))))

(if (= 6 0);; 1 remainder is evaluated
    6
    (gcd (remainder 206 40)(remainder 40 (remainder 260 40))))

(if (= (remainder 206 40)(remainder 40 (remainder 260 40)) 0)
    (remainder 206 40)(remainder 40 (remainder 260 40))
(gcd (remainder 40 (remainder 260 40)) ((remainder 206 40)(remainder 40 (remainder 260 40)))))


(if (= 4 0);; 2 remainders are evaluated
    4
    ((gcd (remainder 40 (remainder 260 40)) ((remainder 206 40)(remainder 40 (remainder 260 40))))))

;; This pattern continues until ..

(if (= 0 0) ;; 7 remainders evaluated
    (remainder (remainder 206 40)
               (remainder 40 (remainder 206 40)))
    (gcd (remainder (remainder 40 (remainder 206 40))
                    (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40))))
         (remainder (remainder (remainder 206 40)
                               (remainder 40 (remainder 206 40)))
                    (remainder (remainder 40 (remainder 206 40))
                               (remainder (remainder 206 40)
                                          (remainder 40
                                                     (remainder 206 40)))))))
