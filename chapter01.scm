;; Building abstractions with procedures

;; Exercise 1.1
;; Below is a sequence of expressions. What is the result printed by the interpreter in response
;; to each expression? Assume that the sequence is to be evaluated in the order in which it is presented.
10 ;; 10
(+ 5 3 4) ;; 12
(- 9 1) ;; 8
(/ 6 2) ;; 3
(+ (* 2 4) (- 4 6)) ;; 6
(define a 3) ;; a
(define b (+ a 1)) ;; b
(+ a b (* a b)) ;; 19
(= a b) ;; #f
(if (and (> b a) (< b (* a b)))
    b
    a) ;; 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ;; 16
(+ 2 (if (> b a) b a)) ;; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ;; 16

;; Exercise 1.2
(/ (+ 5 4 (- 2 
             (- 3 
                (+ 6 
                   (/ 4 3)))))
   (* 3 
      (- 6 2) 
      (- 2 7)))

;; Exerceise 1.3
;; Define a procedure that takes three numbers as arguments and returns the sum of the
;; squares of the two larger numbers.

;; Square
(define (square a)
  (* a a))

;; Sum of squares
(define (sum-of-squares a b)
  (+ (square a)
     (square b)))

(define (sum-sq-larger-two a b c)
  (cond ((and (<= a b) (<= a c)) (sum-of-squares b c)) 
        ((and (<= b a) (<= b c)) (sum-of-squares a c)) 
        ((and (<= c a) (<= c b)) (sum-of-squares a b))))

(sum-sq-larger-two 3 4 5)

;; Exercise 1.4
;; Observe that our model of evaluation allows for combinations whose operators are
;; compound expressions. Use this observation to describe the behavior of the following procedure:
;; (define (a-plus-abs-b a b)
;;   ((if (> b 0) + -) a b))
(define (abs a)
  (if (< a 0)
      (- a)
      a))
;; (abs -2)

(define (a-plus-abs-b a b)
  (+ a 
     (abs b)))

(a-plus-abs-b -4 7)

;; Exercise 1.7
;; The good-enough? test used in computing square roots will not be very effective for
;; finding the square roots of very small numbers. Also, in real computers, arithmetic operations are almost
;; always performed with limited precision. This makes our test inadequate for very large numbers. Explain
;; these statements, with examples showing how the test fails for small and large numbers. An alternative
;; strategy for implementing good-enough? is to watch how guess changes from one iteration to the
;; next and to stop when the change is a very small fraction of the guess. Design a square-root procedure that
;; uses this kind of end test. Does this work better for small and large numbers?

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt-new x guess diff)
  (if (good-enough? diff)
      guess
      (sqrt-new x 
                (improve guess x) 
                (find-change guess 
                             (improve guess x)))))

(define (find-change old improved)
  (abs (- old improved)))

(define (good-enough? diff)
  (and (< diff 0.001) (> diff 0)))

(define (improve guess x)
  (average guess (/ x guess)))

;; 1.10
;; Ackermann function 
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(A 1 10) ;; 1024
(A 2 4) ;; 65536
(A 3 3) ;; 65536

(define (f n) (A 0 n)) ;; 2n
(define (g n) (A 1 n)) ;; (0|n=0) ; (2^n|n>0), 
(define (h n) (A 2 n)) ;; (0|n=0) ; (2|n=1) ; (2^h(n-1))
