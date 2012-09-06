;; Exercise 2.10. Ben Bitdiddle, an expert systems programmer, looks
;; over Alyssa's shoulder and comments that it is not clear what it
;; means to divide by an interval that spans zero. Modify Alyssa's
;; code to check for this condition and to signal an error if it
;; occurs

(load "ex7.scm")


(define a (make-interval 2.3 5))
(define b (make-interval -1.3 1.7))


(define (reciprocal-interval y)
  (make-interval (/ 1.0 (upper-bound y))
                 (/ 1.0 (lower-bound y))))


(define (div-interval x y)
  (mul-interval x (reciprocal-interval y)))


;; reciprocal of a
(reciprocal-interval a)
;Value 17: (.2 . .4347826086956522)

;; reciprocal of b
(reciprocal-interval b)
;Value 18: (.5882352941176471 . -.7692307692307692)

;; interval b spans 0 and the reciprocal of b is not correct as
;; upper-bound is negative while the lower bound is positive

;; check is interval spans zero
(define (spans-zero? x)
  (and (<= (lower-bound x) 0)
       (>= (upper-bound x) 0)))

(spans-zero? b)
;Value: #t
(spans-zero? a)
;Value: #f


(define (div-interval x y)
  (if (spans-zero? y)
      (error "Second interval should not span zero")
      (mul-interval x
                    (reciprocal-interval y))))


(div-interval a b)

