;; Exercise 2.34. Evaluating a polynomial in x at a given value of x
;; can be formulated as an accumulation. We evaluate the polynomial
;; using a well-known algorithm called Horner's rule, which structures
;; the computation as In other words, we start with an, multiply by x,
;; add an-1, multiply by x, and so on, until we reach a0.16 Fill in
;; the following template to produce a procedure that evaluates a
;; polynomial using Horner's rule. Assume that the coefficients of the
;; polynomial are arranged in a sequence, from a0 through an.

;; (define (horner-eval x coefficient-sequence)
;;   (accumulate (lambda (this-coeff higher-terms) <??>)
;;               0
;;               coefficient-sequence))

;; For example, to compute 1 + 3x + 5x3 + x5 at x = 2 you would
;; evaluate (horner-eval 2 (list 1 3 0 5 0 1))


(load "scratch/composition.scm")

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence)) 


(horner-eval 2 (list 1 3 0 5 0 1))
;Value: 79

;;; Explanation

;; higher-terms = how many higher terms are there in the equation.

;; For equation 1 + 3x + 5x^3 + x^5, the order of the equation is 5.

;; When this-coeff is the last elem ie coeff for x^5, higher-terms is 0.

;; When this-coeff is first elem ie. for x^0, higher-terms is 5

;; Let's see how recursion occurs,

;; (horner-eval x (list a0 a1 a2 a3 a4 a5))

;; Starting from when the limiting condition is reached,

;; (op a5 0) 
;; (op a4 a5)
;; (op a3 a5x+a4)
;; (op a2 (a5x+a4)x+a3)
;; (op a1 ((a5x+a4)x+a3)x+a2)
;; (op a0 (((a5x+a4)x+a3)x+a2)x + a1)
;; a0 + ((((a5x+a4)x+a3)x+a2)x + a1)x

;; Finally, a5 is multiplied 5 times by x which is what we want


