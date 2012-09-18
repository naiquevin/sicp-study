;; Exercise 2.13. Show that under the assumption of small percentage
;; tolerances there is a simple formula for the approximate percentage
;; tolerance of the product of two intervals in terms of the
;; tolerances of the factors. You may simplify the problem by assuming
;; that all numbers are positive.

(load "ex12.scm")

;; (a . p1), (a . p2)
;;
;; let vp1 = p1/a*100 and vp2 = p2/a*100
;;
;; (a vp1), (b vp2)
;; (a+vp1, a-vp1), (b+vp2, b-vp2)
;; 
;; a > b
;; so, multiple = ((min * min) . (max * max))
;; ((a-vp1)*(b-vp2)). ((a+vp1)*(b+vp2))

;; lower: ab - a*vp2 - b*vp1 + vp1*vp2
;; upper: ab + a*vp2 + b*vp1 + vp1*vp2

;; vp1*vp2 -> 0 so can be neglected

;; Multiple of the intervals in terms of center and percent
;; center: ab
;; percent: (a*vp2 + b*vp1)/ab * 100


;; Based on the above observation, the percentage tolerance of the
;; product of two intervals (assuming all numbers are positive) can be
;; formulated as following procedure

(define (percent-tolerance-mul a b)
  (let ((ca (center a))
        (cb (center b)))
    (let ((pa (value-from-percent (percent a) ca))
          (pb (value-from-percent (percent b) cb)))
      (* (/ (+ (* ca pb)
               (* cb pa))
            (* ca cb))
         100))))

(define a (make-center-percent 2 1))
(print-interval a)
1.98-2.02
;Value 12: (1.98 . 2.02)

(define b (make-center-percent 5 2))
(print-interval b)
4.9-5.1
;Value 13: (4.9 . 5.1)

(define c (mul-interval a b))
(print-interval c)
9.702-10.302
;Value 14: (9.702 . 10.302)

(percent-tolerance-mul a b)
;Value: 2.999999999999994

