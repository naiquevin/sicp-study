;; Exercise 2.8. Using reasoning analogous to Alyssa's, describe how
;; the difference of two intervals may be computed. Define a
;; corresponding subtraction procedure, called sub-interval.

(load "scratch/interval.scm")

;; a = 1.5 to 2.5
;; b = 4.2 to 4.6

;; 4.2 - 2.5 = 1.5
;; 4.6 - 1.5 = 3.1

;; a = 1.5 to 2.5
;; b = 1.8 to 2.2

;; 1.8 - 2.5 = -0.7
;; 2.2 - 1.5 = 0.7

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(define x (make-interval 1.5 2.5))
(define y (make-interval 1.8 2.2))
(define diff (sub-interval x y))

(print-interval x)
(print-interval y)
(print-interval diff)

(lower-bound diff)
;Value: -.7000000000000002

(upper-bound diff)
;Value: .7

