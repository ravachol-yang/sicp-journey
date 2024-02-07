;; Exercise 2.35:
;; Redefine count-leaves from 2.2.2 as an accumulation:

(define (count-leaves t)
  (accumulate + 0 (map (lambda (t)
                               (if (pair? t)
                                   (count-leaves t)
                                   1))
                        t)))

(define (map proc items)
  (if (null? items)
      nil
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op
                      initial
                      (cdr sequence)))))
