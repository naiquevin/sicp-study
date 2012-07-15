
;; Function Smoothing

;; Exercise 1.44. The idea of smoothing a function is an important
;; concept in signal processing. If f is a function and dx is some
;; small number, then the smoothed version of f is the function whose
;; value at a point x is the average of f(x - dx), f(x), and f(x +
;; dx). Write a procedure smooth that takes as input a procedure that
;; computes f and returns a procedure that computes the smoothed f. It
;; is sometimes valuable to repeatedly smooth a function (that is,
;; smooth the smoothed function, and so on) to obtained the n-fold
;; smoothed function. Show how to generate the n-fold smoothed
;; function of any given function using smooth and repeated from
;; exercise 1.43.

(define dx 0.00001)

(define (sum-of-list ls)
  (if (null? ls)
      0
      (+ (car ls)
         (sum-of-list (cdr ls)))))

(define (average-of-list ls)
  (/ (sum-of-list ls)
     (length ls)))

;; (average-of-list '(1 2 3))
; Value: 2

(define (smooth f)
  (lambda (x)
    (average-of-list
     (list (f (- x dx))
           (f x)
           (f (+ x dx))))))


(square 3)
;Value: 9

((smooth square) 3)
;Value: 9.000000000066665

;; n-fold smooth
(define (n-fold-smooth n)
  (lambda (f)
    ((repeated smooth n) f)))

(((n-fold-smooth 5) square) 3)

