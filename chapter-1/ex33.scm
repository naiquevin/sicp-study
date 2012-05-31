;; Filtered Accumulate

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner result (term a))))
          (else (iter (next a) result))))
  (iter a null-value))


;; Part a

(load "ex21.scm") ; for smallest-divisor function

(define (prime? n)
  (and (not (= n 1)) (= n (smallest-divisor n))))

(define (square x)
  (* x x))

(define (sum a b)
  (+ a b))

(define (inc x)
  (+ x 1))

(define (sum-square-prime a b)
  (filtered-accumulate prime? sum 0 square a inc b))

(sum-square-prime 0 10) ; 87


;; Part 2

(define (idx x)
  x)

(define (product a b)
  (* a b))

(define (prod-rel-prime n)
  (define relative-prime?
    (lambda (a)
    (not (divides? n a))))
  (filtered-accumulate relative-prime? product 1 idx 1 inc n))


(prod-rel-prime 10) ; 36288

