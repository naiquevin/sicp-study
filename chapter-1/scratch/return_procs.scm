
;; Section 1.3.4 Procedures as return values

(define (average a b)
  (/ (+ a b) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (square x)
  (* x x))

((average-damp square) 10)

(load "scratch/fixedpoint.scm")

(define (sqrt x)
  (fixed-point 
   (average-damp (lambda (y) (/ x y))) ; func
   1.0)) ; guess

(sqrt 25)

(define (cube-root x)
  (fixed-point
   (average-damp (lambda (y) (/ x (square y)))) ; func
   1.0)) ; guess

(cube-root 125)

;; Newton's method

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

; 5x^2 + 2x + 3 at x = 4

(define (z x)
  ((deriv (lambda (x) 
            (+ (* 5 (square x))
               (* 2 x)
               3)))
   x))

(z 1) ; ~ 10 * 1 + 2

; x3 at 5
(define (cube x)
  (* x x x))

((deriv cube) 5.0)


;; Newton's transform

(define (newton-transform g)
  (lambda (x) 
    (- x (/ (g x) 
            ((deriv g) x)))))


(define (newtons-method g guess)
  (fixed-point (newton-transform g)
               guess))


(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

(sqrt 100)


;; Higher abstraction

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g)
               guess))


;; Version 1 - using average damping

(define (sqrt x)
  (fixed-point-of-transform 
   (lambda (y) (/ x y))
   average-damp
   1.0))

(sqrt 100)
(sqrt 625)


;; Version 2 - using newton's transform

(define (sqrt x)
  (fixed-point-of-transform
   (lambda (y) (- (square y) x))
   newton-transform
   1.0))

(sqrt 100)
(sqrt 625)


