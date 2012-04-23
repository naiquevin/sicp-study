
;; find if n is prime 
;; if a**n%n == a, then try with more values of a

(define (square x)
  (* x x))

;; fast-exp
(define (exp x n)
  (cond ((= n 0) 1)
        ((even? n) (exp (square x) (/ n 2)))
        (else (* x (exp x (- n 1))))))

(define (expmod x n m)
  (cond ((= n 0) 1)
        ((even? n) 
         (remainder 
          (square (expmod x (/ n 2) m)) 
          m))
        (else (remainder 
               (* x (expmod x (- n 1) m)) 
               m))))

;; fermat's test
(define (fermat n)
  (define (tryit a)
    (= a (expmod a n n)))
  (if (> n 1)
      (tryit (+ 1 (random (- n 1))))
      #f))
  

;; check for fermat's test until false
;; or true for no. of random tests
(define (fast-prime? n times)
  (cond ((= 0 times) #t)
        ((fermat n) (fast-prime? n (- times 1)))
        (else #f)))



