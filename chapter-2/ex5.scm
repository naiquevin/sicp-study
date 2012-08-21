;; Exercise 2.5. Show that we can represent pairs of nonnegative
;; integers using only numbers and arithmetic operations if we
;; represent the pair a and b as the integer that is the product 2a
;; 3b. Give the corresponding definitions of the procedures cons, car,
;; and cdr.

;; a = 3, b = 2 
;; 2^a * 3^b
;; 8 * 9
;; 72

;; divide 72 by 3: 72 -> 24 -> 8 - log of 8 to base 2
;; divide 72 by 2: 72 -> 36 -> 18 -> 9  - log 9 to base 3

(define (nthpower x n)
  (define (iter x n result)
    (if (= n 0)
        result
        (iter x (- n 1) (* result x))))
  (iter x n 1))

(define (power-of x)
  (lambda (n)
    (nthpower x n)))

(define (divide-until-multiple n d)
  (let ((q (/ n d))
        (r (remainder n d)))
    (if (not (= r 0))
        n
        (divide-until-multiple q d))))

(define (log-to-base n)
  (lambda (x)
    (/ (log x) (log n))))

(define log-to-two (log-to-base 2))
(define log-to-three (log-to-base 3))


(define (cons a b)
  (* ((power-of 2) a)
     ((power-of 3) b)))

(define (car c)
  (log-to-two (divide-until-multiple c 3)))

(define (cdr c)
  (log-to-three (divide-until-multiple c 2)))

(define c (cons 3 2)) ; 72
(= (car c) 3)
;Value: #t
(= (cdr c) 2)
;Value: #t

