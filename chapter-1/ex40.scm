;; Exercise 1.40. Define a procedure cubic that can be used together
;; with the newtons-method procedure in expressions of the form
;; (newtons-method (cubic a b c) 1) to approximate zeros of the cubic
;; x3 + ax2 + bx + c.


;; The fixed-point, newtons-method and other procedures which are
;; undefined here are defined in scratch/fixedpoint.scm and
;; scratch/return_procs.scm so make sure those files are loaded before
;; evaluating the following expressions

;; We need to solve for x^3 + ax^2 + bx + c = 0

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))


(define (zero-of-cubic a b c)
  (newtons-method (cubic a b c) 1.0))


(zero-of-cubic 1 2 3)
;Value: -1.2756822036498454

(zero-of-cubic 3 4 5)
;Value: -2.213411667805671

;; cross verifying
((cubic 1 2 3) -1.2756822036498454)

((cubic 3 4 5) -2.213411667805671)


;; Just confirming with billthelizard's solutions
;; http://www.billthelizard.com/2010/08/sicp-140-zeros-of-cubic-function.html

(zero-of-cubic 3 -2.4 6)
;Value: -3.9813366488302706

; Other solutions also confirmed using cubic calculator -
; http://www.1728.org/cubic.htm

