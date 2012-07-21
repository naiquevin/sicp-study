;; iterative-improve

;; Exercise 1.46. Several of the numerical methods described in this
;; chapter are instances of an extremely general computational strategy
;; known as iterative improvement. Iterative improvement says that, to
;; compute something, we start with an initial guess for the answer, test
;; if the guess is good enough, and otherwise improve the guess and
;; continue the process using the improved guess as the new guess. Write
;; a procedure iterative-improve that takes two procedures as arguments:
;; a method for telling whether a guess is good enough and a method for
;; improving a guess. Iterative-improve should return as its value a
;; procedure that takes a guess as argument and keeps improving the guess
;; until it is good enough.  Rewrite the sqrt procedure of section 1.1.7
;; and the fixed-point procedure of section 1.3.3 in terms of
;; iterative-improve.


(define (average a b)
  (/ (+ a b) 2))

(define (iterative-improve good-enough? improve-guess)
  (define (keep-improving guess)
    (if (good-enough? guess)
        guess
        (keep-improving (improve-guess guess))))
  (lambda (guess)
    (keep-improving guess)))


;; sqrt using iterative-improve

(define (sqrt x)
  (define good-enough?
    (lambda (y)
      (< (abs (- (square y) x)) 0.001)))
  (define improve
    (lambda (y)
      (average y (/ x y))))
  ((iterative-improve good-enough? improve) 1.0))


(sqrt 10)
(sqrt 625)

;; fixed-point using iterative-improve

(define (fixed-point f guess)
  (define tolerance 0.0001)
  (define good-enough?
    (lambda (y)
      (< (abs (- y (f y))) tolerance)))
  (define improve
    (lambda (y)
      (f y)))
  ((iterative-improve good-enough? improve) 1.0))


(fixed-point cos 1.0)

(define (another-sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(another-sqrt 100)

