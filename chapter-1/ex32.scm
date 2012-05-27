;; Accumulate

;; Part a (Recursive Process)

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner 
       (term a) 
       (accumulate combiner null-value term (next a) next b))))


(define (idx x)
  x)

(define (inc x)
  (+ 1 x))


(define (add a b)
  (+ a b))

(define (multiply a b)
  (* a b))


(accumulate add 0 idx 1 inc 10) ; 55

(accumulate multiply 1 idx 1 inc 5) ; 120


;; Part b (Iterative process)

(define (accumulate-i combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))


(accumulate-i add 0 idx 1 inc 10) ; 55

(accumulate-i multiply 1 idx 1 inc 5) ; 120
(accumulate-i multiply 1 idx 6 inc 10); 30240

