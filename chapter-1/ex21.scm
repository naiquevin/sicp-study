;; Exercise 1.21. Use the smallest-divisor procedure to find the smallest divisor of each of the
;; following numbers: 199, 1999, 19999.

(define (smallest-divisor n)
  (find-smallest-divisor n 2))

(define (divides? n a)
  (= 0 (remainder n a)))

(define (find-smallest-divisor n a)
  (cond ((> (square a) n) n)
        ((divides? n a) a)
        (else (find-smallest-divisor n (+ a 1)))))


(smallest-divisor 199)
;; 199

(smallest-divisor 1999)
;; 1999

(smallest-divisor 19999)
;; 7

