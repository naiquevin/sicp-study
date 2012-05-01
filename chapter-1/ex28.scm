;; Miller-Rabin test

;; Disclaimer: While I could write the procedure as per the 
;; steps mentioned in the problem, I don't 
;; think I have understood the algorithm itself quite well
;; and the explanation I found at other sources differs from
;; the SICP one.

(define (non-trivial-sqrt? a n)
  ;; a number not equal to 1 or n - 1 whose square is 
  ;; congruent to 1 modulo n
  ;; ie. a**2%n = 1
  (and (not (= a 1))
       (not (= a (- n 1)))
       (= 1 (remainder (square a) n))))


(define (expmod a n m)
  (cond ((= n 0) 1)
        ((even? n)
         ;; enter 'let'!
         (let ((tmp (square (expmod a (/ n 2) m))))
           (if (non-trivial-sqrt? tmp n)
               0
               (remainder tmp m))))
        (else (remainder (* a (expmod a (- n 1) m)) m))))


(define (miller-rabin n)
  (define (try-it a)
    (not (= (expmod a (- n 1) n) 0)))
  (if (> n 1)
      (try-it (+ 1 (random (- n 1))))
      #f))


(define (foolproof-prime? n times)
  (cond ((= 0 times) #t)
        ((miller-rabin n) (foolproof-prime? n (- times 1)))
        (else #f)))


(define (test-carmicheal ns r)
  ;; (display ns)
  ;; (newline)
  (cond ((null? ns) r)
        (else (test-carmicheal
               (cdr ns)
               (cons 
                (foolproof-prime? (car ns) 10)
                r)))))


(test-carmicheal '(561 1105 1729 2465 2821 6601) '())

;; > (#f #f #f #f #f #f)

