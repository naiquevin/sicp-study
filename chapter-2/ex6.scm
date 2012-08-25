;; Exercise 2.6. In case representing pairs as procedures wasn't
;; mind-boggling enough, consider that, in a language that can
;; manipulate procedures, we can get by without numbers (at least
;; insofar as nonnegative integers are concerned) by implementing 0
;; and the operation of adding 1 as

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;; This representation is known as Church numerals, after its
;; inventor, Alonzo Church, the logician who invented the calculus.

;; Define one and two directly (not in terms of zero and
;; add-1). (Hint: Use substitution to evaluate (add-1 zero)). Give a
;; direct definition of the addition procedure + (not in terms of
;; repeated application of add-1).

(add-1 zero)
(lambda (f) (lambda (x) (f x))) ; zero returns func that returns
                                ; identity func

;; one
(define one (lambda (f) (lambda (x) (f x))))

(add-1 one)
(lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
(lambda (f) (lambda (x) (f (f x))))

;; two
(define two (lambda (f) (lambda (x) (f (f x)))))

;; n can be represented as a function that takes a function 'f'
;; and returns a function that takes an arg 'x' and applies 'f' 
;; n times to 'x'

;; From the add-1 example,
((n f) x) ; is a function call that applies 'f' n times to 'x'

(f ((n f) x)) ; applying 'f' "one" more time to value returned by
              ; above fn call

;; which is same as
((one f) ((n f) x))

;; proof
((lambda (x) (f x)) ((n f) x))
(f ((n f) x))

;; so add-2 to n will be
((two f) ((n f) x))

;; so add-m to n
((m f) ((n f) x))

;; plus
(define (plus n m)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))


;; concrete examples for understanding taken from
;; http://www.billthelizard.com/2010/10/sicp-26-church-numerals.html
;; (ofcourse after coming up with correct ans :-)

(define (inc a)
  (+ a 1))

((zero inc) 1) ; inc 1 by zero
;Value: 1
((one inc) 1) ; inc 1 by one
;Value: 2
((two inc) 1) ; inc 1 by two
;Value: 3

(define three (plus two one))
((three inc) 1) ; inc 1 by three
;Value: 4

