
;; Exercise 2.7 Selectors for Alyssa P. Hacker's interval arithmatic
;; system (section 2.1.4)

(load "scratch/interval.scm")

(define (make-interval a b)
  (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))


(define intvl (make-interval 9.7 10.3))
(print-interval intvl)

(and (= (upper-bound intvl) 
        10.3)
     (= (lower-bound intvl)
        9.7))

