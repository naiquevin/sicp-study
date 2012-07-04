
;; Half interval method example

(define (average a b)
  (/ (+ a b) 2))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint))))))

(define (close-enough? a b)
  (< (abs (- a b)) 0.001))


(define (half-interval-method f neg-point pos-point)
  (let ((a (f neg-point))
        (b (f pos-point)))
    (cond ((and (negative? a) (positive? b))
           (search f neg-point pos-point))
          ((and (positive? a) (negative? b))
           (search f pos-point neg-point))
          (else 
           (error "Values are not of opposite sign" a b)))))


(half-interval-method sin 2.0 4.0)

(half-interval-method sin 1.0 2.0)

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0)
 
