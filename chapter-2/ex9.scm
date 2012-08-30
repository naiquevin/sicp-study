;; Exercise 2.9. The width of an interval is half of the difference
;; between its upper and lower bounds. The width is a measure of the
;; uncertainty of the number specified by the interval. For some
;; arithmetic operations the width of the result of combining two
;; intervals is a function only of the widths of the argument
;; intervals, whereas for others the width of the combination is not a
;; function of the widths of the argument intervals. Show that the
;; width of the sum (or difference) of two intervals is a function
;; only of the widths of the intervals being added (or
;; subtracted). Give examples to show that this is not true for
;; multiplication or division.

(load "scratch/interval.scm")
(load "ex7.scm")
(load "ex8.scm")

(define (interval-width interval)
  (/ (- (upper-bound interval)
        (lower-bound interval))
     2))

(define interval-1 (make-interval 9.7 10.3))
(define interval-2 (make-interval 4.5 6.7))
(interval-width interval-1)
;Value: .3000000000000007

(interval-width interval-2)
;Value: 1.1

;; to show that width of sum and difference of two intervals is a
;; function of the width of the intervals

(define interval-sum (add-interval interval-1 interval-2))
interval-sum
;Value 13: (14.2 . 17.)

(interval-width interval-sum)
;Value: 1.4000000000000004

;; which is nothing but sum of the widths of individual intervals
(+ 1.1 0.3)

(define interval-diff (sub-interval interval-1 interval-2))

interval-diff
;Value 14: (2.999999999999999 . 5.800000000000001)

(interval-width interval-diff)
;Value: 1.4000000000000008

;; The reason the combined with is the same in case of subtraction is
;; that whan diff of two intervals is found, the range becomes sum of
;; ranges of the two intervals

(define (interval-width-combined x y)
  (+ (interval-width x)
     (interval-width y)))

(interval-width-combined interval-1 interval-2)
;Value: 1.4000000000000008

;; To verify, let us take two intervals with different upper and lower
;; bounds but the same width

(define interval-3 (make-interval 6.7 7.3))
(define interval-4 (make-interval 4.1 6.3))

(define interval-sum2 (add-interval interval-3 interval-4))
interval-sum2
;Value 21: (10.8 . 13.6)

(interval-width interval-sum2)
;Value: 1.3999999999999995
(interval-width-combined interval-3 interval-4)
;Value: 1.4

;; But it's not the case with multiplication and division

(define interval-prod (mul-interval interval-1 interval-2))
interval-prod
;Value 16: (43.65 . 69.01)

(interval-width interval-prod)
;Value: 12.680000000000003

(define interval-prod2 (mul-interval interval-3 interval-4))
;Value 22: (27.47 . 45.989999999999995)

(interval-width interval-prod2)
;Value: 9.259999999999998

(define interval-ratio (div-interval interval-1 interval-2))
interval-ratio
;Value 18: (1.4477611940298505 . 2.2888888888888888)

(interval-width interval-ratio)
;Value: .42056384742951913

(define interval-ratio2 (div-interval interval-3 interval-4))
interval-ratio
;Value 18: (1.4477611940298505 . 2.2888888888888888)

(interval-width interval-ratio2)
;Value: .35849787069299266

