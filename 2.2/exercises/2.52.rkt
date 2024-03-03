#lang sicp
;; Exercise 2.52: Make changes to the square limit of wave shown in Figure 2.9
;; by working at each of the levels described above. In particular:

;;  Add some segments to the primitive wave painter of Exercise 2.49 (to add a smile, for example).

;; Change the pattern constructed by corner-split (for example,
;; by using only one copy of the up-split and right-split images instead of two).


;; Modify the version of square-limit that uses square-of-four so as
;; to assemble the corners in a different pattern.
;; (For example, you might make the big Mr. Rogers look outward from each corner of the square.) 

(#%require sicp-pict)

;; right split
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter 
                                  (- n 1))))
        (beside painter 
                (below smaller smaller)))))

;; up split
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter
                               (- n 1))))
        (below painter
               (beside smaller smaller)))))

;; corner split
(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter 
                                (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right 
                                   right))
              (corner (corner-split painter 
                                    (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right 
                         corner))))))
