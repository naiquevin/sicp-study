;; Product procedure analogous to sum

;; Part a

(define (naive-product a b)
  (if (> a b)
      1
      (* a (naive-product (+ a 1) b))))


(define (product term a next b)
  (if (> a b)
      1
      (* (term a) (product term (next a) next b))))


(define (idx a) 
  a)


(define (inc a) 
  (+ a 1))


;; Factorial procedure in terms of product
(define (factorial n)
  (product idx 1 inc n))


;; Approx. of pi using product using John Wallis method

(define (pi-approx n)
  (define (square x)
    (* x x))
  (define term
    (lambda (i)
      (if (odd? i)
          (/ (+ i 1) (+ i 2))
          (/ (+ i 2) (+ i 1)))))
  (* 4.0 (product term 1 inc n)))

(pi-approx 10) ; 3.2751010413348074
(pi-approx 100) ; 3.1570301764551676
(pi-approx 1000) ; 3.1431607055322663
(pi-approx 10000) ; 3.1417497057380523 OK

;;
;; Approach 1(failed)
;; 
;; Calculating the products of numerator and denominator separately
;; and dividing
;;
;; num = 2.4.4.6.6.8.8...
;;     = 2 . product[2..infinity] (2i)^2
;; den = 3.3.5.5.7.7...
;;     = product[1..infinity] (2i+1)^2
;;
;; However, this approach gives answers of the order
;; 1.5696190634834557e-3 for n = 1000
;; This fails, probably because of the extra 2 in the num.

;; Approach 2 - 
;;
;; Calculating product of fractions
;; 
;; i=1 => 2/3 (i+1)/(i+2)
;; i=2 => 4/3 (i+2)/(i+1)
;; i=3 => 4/5 (i+1)/(i+2)
;; ..
;; This works


;; Part b

(define (product-i term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

