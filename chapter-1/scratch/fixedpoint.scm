
(define tolerance 0.0001)

(define (fixed-point f guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      ;; (display next)
      ;; (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try guess))

(fixed-point cos 1.0)

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(sqrt 4)
(sqrt 25)
  
