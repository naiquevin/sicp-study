;; Exercise 2.12. Define a constructor make-center-percent that takes
;; a center and a percentage tolerance and produces the desired
;; interval. You must also define a selector percent that produces the
;; percentage tolerance for a given interval. The center selector is
;; the same as the one shown above.

(load "ex7.scm")


(define (make-center-width c w)
  (make-interval (- c w)
                 (+ c w)))


(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))


(define (width i)
  (/ (- (upper-bound i)
        (lower-bound i))
     2))


;; convert % of total to value
(define (value-from-percent percent total)
  (* (/ percent 100.0) total))


;; convert value per total to value per cent
(define (percent-from-value value total)
  (* (/ value total) 100.0))


(define (make-center-percent c p)
  (let ((tolerance (value-from-percent p c)))
    (make-interval (- c tolerance)
                   (+ c tolerance))))


;; selector to get the percentage of tolerance
(define (percent i)
  (let ((c (center i))
        (lower (lower-bound i)))
    (percent-from-value (- c lower) c)))


(define i (make-center-percent 5 10))

(print-interval i)
;Value 14: (4.5 . 5.5)

(equal? (center i) 5.0)
;Value: #t

(equal? (percent i) 10.0)
;Value: #t

