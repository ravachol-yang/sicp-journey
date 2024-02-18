;; sum up
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;; integration using simpson's rule
(define (simpson f a b n)
  ;; next n
  (define (next n)
    (+ n 1))
  ;; "h" in the formula, h=(b-a)/n
  (define h (/ (- b a) n))
  ;; "y" in the formula, y=f(a + kh)
  (define (y k)
    (f (
      + a (* k h))))
;; factor before every term
  (define (factor k)
    (cond ((or (= k 1) (= k n))
      1)
      ((odd? k)
      2)
      (else 4)))
;; every term
  (define (term k)
    (* (factor k)
      (y k)))
;;   sum
  (* (/ h 3) (sum term 0 next n))
)

(define (cube n) (* n n n))
