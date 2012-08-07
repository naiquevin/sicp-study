;; Exercise 2.1. Define a better version of make-rat that handles both
;; positive and negative arguments.  Make-rat should normalize the sign
;; so that if the rational number is positive, both the numerator and
;; denominator are positive, and if the rational number is negative, only
;; the numerator is negative.

(load "scratch/ratarith.scm")

(define (make-positive n)
  (abs n))


(define (make-negative n)
  (- 0 (abs n)))


;; better version of make-rat that normalizes the signs of n and d
(define (make-rat n d)
  (cond ((positive? (/ n d)) 
         (cons (make-positive n)
               (make-positive d)))
        ((negative? (/ n d))
         (cons (make-negative n)
               (make-positive d)))
        (else (cons n d))))


(print-rat (make-rat 3 4))
3/4

(print-rat (make-rat -3 -4))
3/4

(print-rat (make-rat -3 4))
-3/4

(print-rat (make-rat 3 -4))
-3/4

