;; Exercise 2.14. Demonstrate that Lem is right. Investigate the
;; behavior of the system on a variety of arithmetic expressions. Make
;; some intervals A and B, and use them in computing the expressions
;; A/A and A/B. You will get the most insight by using intervals whose
;; width is a small percentage of the center value.  Examine the
;; results of the computation in center-percent form (see exercise
;; 2.12).


(load "scratch/interval.scm")
(load "ex12.scm")

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))


(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))


(define r1 (make-interval 2.3 3.2))
(define r2 (make-interval 4.1 4.5))

(par1 r1 r2)
;Value 12: (1.2246753246753244 . 2.25)

(par2 r1 r2)
;Value 13: (1.4734375 . 1.87012987012987)

;; Lem is right as we can see that there is a some difference in the
;; parallel resistance value computed by the two procedures.


;; Check for intervals defined in center percent form with small
;; intervals

(define r3 (make-center-percent 5 0.01))
;; r3 = (4.9995 . 5.0005)
(define r4 (make-center-percent 7 0.05))
;; r4 = (6.9965 . 7.0035)

(define pr1 (par1 r3 r4))
;Value 19: (2.9139454973342223 . 2.9193899424808265)

(define pr2 (par2 r3 r4))
;Value 20: (2.9158887754251417 . 2.9174443310563145)

;; After trying out another example, it can be observed that value of
;; parallel resistance computed by the second procedure is narrow and
;; 'lies inside' of the one computed by the first procedure

(define r3r3 (div-interval r3 r3))
;Value 33: (.9998000199980004 . 1.000200020002)

(define r4r4 (div-interval r4 r4))
;Value 34: (.9990004997501251 . 1.001000500250125)

(define r3r4 (div-interval r3 r4))
;Value 35: (.7138573570357679 . .7147145001071965)

;; Let us see center of division of two same intervals. We expect them
;; to to return 1 as the answer but actually it isn't so

(center r3r3)
;Value: 1.0000000200000003

(center r4r4)
;Value: 1.000000500000125

;; These observations don't tell us much about the reason for such a
;; behaviour but it shall be helpful in the next solution


;; Exercise 2.15. Eva Lu Ator, another user, has also noticed the
;; different intervals computed by different but algebraically
;; equivalent expressions. She says that a formula to compute with
;; intervals using Alyssa's system will produce tighter error bounds
;; if it can be written in such a form that no variable that
;; represents an uncertain number is repeated. Thus, she says, par2 is
;; a ``better'' program for parallel resistances than par1. Is she
;; right? Why?

;; How we came up with the formula for 1st procedure above

;; PR = 1 / ((1/R1) + (1/R2))
;;    = 1 / ((R2 + R1)/R1.R2)

(define one (make-interval 1 1))

(define one-by-r3 (div-interval one r3))
(define one-by-r4 (div-interval one r4))

(add-interval one-by-r3 one-by-r4)
;Value 40: (.3427657519819518 . .34294860916092323)

(div-interval (add-interval r3 r4) (mul-interval r3 r4))
;Value 41: (.34253731762541606 . .34317731780324456)

;; This means that 1/R1 + 1/R2 != (R2 + R1)/R1R2

;; But why is it so? Let us see how we carry out such reduction using
;; algebra for numbers

;; 1/R1 + 1/R2;; 
;; (R1*R2/R1*R2) * (1/R1 + 1/R2) ie, multiply and divide by R1*R2

;; For numbers, X/X == 1 but it's not the case for intervals (refer
;; ex2.14) and so such simplification will not work for intervals

;; Thus, in case of intervals par2 is better than par1 as in par2
;; variables r1 and r2 which in reality can represent a range of
;; values are used only once and not repeated unlike par1.

;; Why par2 results in tighter error bounds?
;; no idea!


;; Exercise 2.16. Explain, in general, why equivalent algebraic
;; expressions may lead to different answers.  Can you devise an
;; interval-arithmetic package that does not have this shortcoming, or
;; is this task impossible? (Warning: This problem is very difficult.)

;; This is because when dealing with numbers in algebraic expressions
;; we are certain about the values that the numbers hold but in case
;; of intervals it's not the case

;; eg. a = (2, 4) and,
;;     b = (3, 6) then,

;;     5 < a + b < 10 since,
;;     a can be anything between 2 and 4
;;     b can be anything between 3 and 6

;; now consider numbers,
;;     a = 2 and,
;;     b = 3 then,
;;     a + b = 5

;; if we are given that a + b = 5 and a*b = 6 then we can find out a
;; and b if a and b are numbers but it's not possible to do so in case
;; of intervals ie. we cannot tell the upper and lower bounds of a and
;; b

;; It might be possible to solve the interval operations using
;; algebraic expressions if it's somehow possible to express the
;; operations as functions of centers and percentage tolerances

;; eg, (only for the sake of an example ;-) if we ignore the percentage
;; tolerance in interval division and only divide the centers, then
;; X/X == 1

(define (div-interval2 a b)
  (/ (center a)
     (center b)))

