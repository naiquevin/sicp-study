;; SICP 1.2.4

;; b^n = 1 for n = 0
;; b^n = b*b^(n-1) for n > 0

;; Recursive solution
(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

;; (expt 3 4)
(expt 3 4)
(* 3 (expt 3 3))
(* 3 (* 3 (expt 3 2)))
(* 3 (* 3 (* 3 (expt 3 1))))
(* 3 (* 3 (* 3 (* 3 (expt 3 0)))))
(* 3 (* 3 (* 3 (* 3 1))))
(* 3 (* 3 (* 3 3)))
(* 3 (* 3 9))
(* 3 27)
81

(expt 256 256)

;; Iterative solution
(define (expt-i b n)
  (expt-iter b n 1))

(define (expt-iter b n multiple)
  (if (= n 0)
      multiple
      (expt-iter b (- n 1) (* b multiple))))

(expt-iter 3 4 1)
(expt-iter 3 3 3)
(expt-iter 3 2 9)
(expt-iter 3 1 27)
(expt-iter 3 0 81)
81

(expt-i 256 256)

;; faster exponentiation

;; (b^(n/2))^2 if n%2 = 0
;; b^n = 1 for n = 0
;; b^n = b*b^(n-1) for n > 0

(define (square x)
  (* x x))

(define (even? x)
  (= (remainder x 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (fast-expt-i b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n a)
  (cond ((= n 0) a)
        ((even? n)
         (fast-expt-iter (square b) (/ n 2) a))
        (else (fast-expt-iter b (- n 1) (* a b)))))

(fast-expt-i 4 6)

(fast-expt-i 2 10)

;; (fei 4 6 1)
;; (fei 16 3 1)
;; (fei 16 2 16)
;; (fei 256 1 16)
;; (fei 236 0 256*16)
;; (* 256 16)
;; 4096
