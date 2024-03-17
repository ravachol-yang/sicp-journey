#lang sicp
;; Exercise 2.73:

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 
               1 
               0))
         (else ((get 'deriv (operator exp)) 
                (operands exp) 
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (variable? exp)
  (symbol? exp))

(define (same-variable? v1 v2)
  (and (variable? v1)
       (variable? v2)
       (eq? v1 v2)))

;; 1.Explain what was done above. Why can’t we assimilate the predicates
;; number? and variable? into the data-directed dispatch?

;; 2.Write the procedures for derivatives of sums and products,
;; and the auxiliary code required to install them in the table used by the program above. 

;; Put and Get from 3.3
 (define (make-table) (list '*table*)) 
 ; 2d table 
 (define (lookup2 key-1 key-2 table) 
     (let ((subtable (assoc key-1 (cdr table)))) 
         (if subtable 
             (let ((record (assoc key-2 (cdr subtable)))) 
                 (if record 
                     (cdr record) 
                     #f 
                 ) 
             ) 
             #f 
         ) 
     ) 
 ) 
 (define (insert2! key-1 key-2 value table) 
     (let ((subtable (assoc key-1 (cdr table)))) 
         (if subtable 
             ; subtable exist 
             (let ((record (assoc key-2 (cdr subtable)))) 
                 (if record 
                     (set-cdr! record value) ; modify record 
                     (set-cdr! subtable 
                               (cons (cons key-2 value) (cdr subtable))) ; add record 
                 ) 
             ) 
             ; subtable doesn't exist, insert a subtable 
             (set-cdr! table 
                       (cons (list key-1 (cons key-2 value)) ; inner subtable 
                             (cdr table)) 
             ) 
         ) 
     ) 
 ) 
  
 ; put and get 
 (define *table* (make-table)) ; a global table 
 (define (put op type item) 
     (insert2! op type item *table*) 
 ) 
 (define (get op type) 
     (lookup2 op type *table*) 
 ) 
