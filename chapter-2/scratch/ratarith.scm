;; Arithmatic of rational numbers

;; constuctors and selectors
(define (make-rat n d)
  (cons n d))


(define (numer x)  
  (car x))


(define (denom x)
  (cdr x))


(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
  #t)


;; Arithmatic operations

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))


(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))


(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))


(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))


(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


(define a (make-rat 1 3))
(define b (make-rat 2 3))

(print-rat (add-rat a b))

(print-rat (sub-rat b a))

(print-rat (mul-rat a b))

(print-rat (div-rat b a))

(define c (make-rat 1 3))

(equal-rat? a c)
(equal-rat? a b)
               
            
