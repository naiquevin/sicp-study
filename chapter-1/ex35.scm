;; Exercise 1.35. Show that the golden ratio (section 1.2.2) is a fixed
;; point of the transformation x |-> 1 + 1/x, and use this fact to
;; compute by means of the fixed-point procedure.

(define tolerance 0.0001)

(define (fixed-point f guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try guess))

;; We need to solve for x such that it satisfies the equation
;; x = 1 + 1/x

;; f = (lambda (x) (+ 1 (/ 1 x)))

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)
;Value: 1.6180555555555556

;; This is equal to the golden ratio
;; (http://en.wikipedia.org/wiki/Golden_ratio)


;; Computing the golden-ratio using fixed-point
(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
                      
(golden-ratio)
;Value: 1.6180555555555556

