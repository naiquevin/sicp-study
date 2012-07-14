
;; Exercise 1.41. Define a procedure double that takes a procedure of
;; one argument as argument and returns a procedure that applies the
;; original procedure twice. For example, if inc is a procedure that
;; adds 1 to its argument, then (double inc) should be a procedure
;; that adds 2. What value is returned by (((double (double double))
;; inc) 5)

(define (double g)
  (lambda (x)
    (g (g x))))


(define (inc x)
  (+ 1 x))

((double inc) 5)
;Value: 7

(((double (double double)) inc) 5)
; Value: 21


(double double)
(lambda (x)
  (double (double x)))

(double (double double))
(lambda (x)
  ((lambda (x)
     (double (double x)))
   ((lambda (x)
      (double (double x)))
    x)))

((double (double double)) inc)

((lambda (inc)
   (double (double inc)))
 ((lambda (inc)
    (double (double inc)))
  inc))

((lambda (inc)
   (double (lambda (x)
             (inc (inc x)))))
 ((lambda (inc)
    (double (lambda (x)
              (inc (inc x)))))
  inc))


;; In above expansion there are 4 calls to inc. There are still two
;; calls to double which will result in total of 16 calls to inc if
;; the expression is further expanded so the answer 5 + 16 = 21

