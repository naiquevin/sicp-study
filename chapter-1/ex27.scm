;; Testing Fermats Little Theorem on Carmicheal numbers
;; as explained in Footnote 47


(define (fermat n)
  (define (try-it a)
    (= a (expmod a n n)))
  (define (try-all-a a)
    (cond ((= a 0) #t)
          ((try-it a) (try-all-a (- a 1)))
          (else #f)))
  (try-all-a (- n 1)))


(define (test-carmicheals c r)
  (cond ((null? c) r)
        (else 
         (test-carmicheals 
          (cdr c)
          (cons (fermat (car c)) r)))))

(test-carmicheals '(561 1105 1729 2465 2821 6601) '())

;; (#t #t #t #t #t #t)

;; Evidently the Carmicheal numbers really do fool the fermat's test!!





  
    



