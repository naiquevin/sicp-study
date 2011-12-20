;; Recursive fibonacci
(define (fib n)
     (cond ((= n 0) 0)
           ((= n 1) 1)
           (else (+ (fib (- n 1)) 
                    (fib (- n 2))))))

;; Iterative fibonacci
(define (fibo n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(fibo 4)

;; sum of fibonacci
(define (fibo-sum n)
  (define (fibo-sum-iter a b count sum)
    (if (= count 0)
        (+ sum b)
        (fibo-sum-iter (+ a b) a (- count 1) (+ sum b))))
  (fibo-sum-iter 1 0 n 0))

;;
;; 1 0   0
;; 1 1 4 1
;; 2 1 3 2
;; 3 2 2 4 
;; 5 3 1 7
;; 8 5 0 12

(fibo-sum 5)






