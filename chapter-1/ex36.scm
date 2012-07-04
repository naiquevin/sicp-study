;; Exercise 1.36.
;;
;; Modify fixed-point so that it prints the sequence of approximations
;; it generates, using the newline and display primitives shown in
;; exercise 1.22. Then find a solution to x^x = 1000 by finding a
;; fixed point of x |-> log(1000)/log(x). (Use Scheme's primitive log
;; procedure, which computes natural logarithms.) Compare the number
;; of steps this takes with and without average damping. (Note that
;; you cannot start fixed-point with a guess of 1, as this would cause
;; division by log(1) = 0.)


(define tolerance 0.0001)

(define (fixed-point f guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (newline)
  (define (try guess)
    (let ((next (f guess)))
      (display next)
      (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try guess))


;; Since we cannot start with 1.0 as our initial guess, we need 
;; to find a sensible guess to start with 
;; 4^4 = 256
;; 5^5 = 3125
;; So 4.0 is a good initial guess for us in this case

(fixed-point 
 (lambda (x)
   (/ (log 1000) 
      (log x)))
 4.0)

;; 4.9828921423310435
;; 4.301189432497896
;; 4.734933901055578
;; 4.442378437719526
;; 4.632377941509958
;; 4.505830646780212
;; 4.588735606875766
;; 4.533824356566501
;; 4.56993352418142
;; 4.546075272637246
;; 4.561789745175654
;; 4.55141783665413
;; 4.5582542120702625
;; 4.553744140202578
;; 4.556717747893265
;; 4.554756404545319
;; 4.5560497413912975
;; 4.5551967522618035
;; 4.555759257615811
;; 4.555388284933278
;; 4.555632929754932
;; 4.555471588998784
;; 4.555577989320218
;; 4.555507819903776
;; ;Value: 4.555507819903776


;; It takes 24 steps normally

;; With average damping

(define (average a b)
  (/ (+ a b) 2))

(fixed-point 
 (lambda (x)
   (average x
   (/ (log 1000) 
      (log x))))
 4.0)

;; 4.491446071165521
;; 4.544974650975552
;; 4.553746974742814
;; 4.555231425802502
;; 4.555483906560562
;; 4.5555268862194875
;; ;Value: 4.5555268862194875

;; With average damping, it takes only 6 steps
               
