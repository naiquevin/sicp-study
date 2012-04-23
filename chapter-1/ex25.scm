;; Alyssa P. Hacker complains that we went to a lot of extra work in writing expmod. After
;; all, she says, since we already know how to compute exponentials, we could have simply written
;; (define (expmod base exp m)
;;   (remainder (fast-expt base exp) m))
;; Is she correct? Would this procedure serve as well for our fast prime tester? Explain.


(define (square n)
  (* n n))

(define (fast-exp x n)
  (cond ((= n 0) 1)
        ((even? n) (fast-exp (square x) (/ n 2)))
        (else (* x (fast-exp x (- n 1))))))


(define (exp-then-rem x n m)
  (remainder (fast-exp x n) m))


(define (expmod x n m)
  (cond ((= n 0) 1)
        ((even? n) 
         (remainder 
          (square (expmod x (/ n 2) m)) 
          m))
        (else (remainder 
               (* x (expmod x (- n 1) m)) 
               m))))


;; timed test XXX inelegant method
(define (timed-test base exp m times)
  (define (iter-fast-exp-then-mod times)
    (if (= 1 times)
        (remainder (fast-exp base exp) m)
        (iter-fast-exp-then-mod (- times 1))))
  (define (iter-exp-mod times)
    (if (= 1 times)
        (expmod base exp m)
        (iter-exp-mod (- times 1))))
  (newline)
  (define s (runtime))
  (display s)
  (newline)
  (iter-fast-exp-then-mod times)
  (display (- (runtime) s))
  (newline)
  (define t (runtime))
  (iter-exp-mod times)
  (display (- (runtime) t))
  (newline))


;; Running in DrRacket..

;; > (timed-test 1487 5 6 1000)

;; 1335118643979938
;; 7706
;; 438


;; > (timed-test 14873434 523 6 10000)

;; 1335119008140718
;; 10374
;; 2055

;; expmod is significantly faster than reducing the problem 
;; to exp and then finding the mod but it does work.
;; So for the fast prime test, this will work as long as 
;; the values are small.

;; eg

;; ]=> (fast-prime? 1434553 10)
;; takes ridiculously longer with (remainder (exp))
;; than (expmod)
