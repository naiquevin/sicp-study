;; Iterative Sum

(define (sum term a next b)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (inc a)
  (+ a 1))

(define (idx a)
  a)

(sum idx 1 inc 10) ; 55
        
