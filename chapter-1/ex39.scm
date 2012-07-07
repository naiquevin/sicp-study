
(define (square x)
  (* x x))

(define pi 3.1416)

;; use cont-frac defined in ex37.scm or ex38.scm

(define (tan-cf x k)
  (cont-frac (lambda (i) 
               (if (= 1 i) 
                   x
                   (- 0 (square x))))
             (lambda (n) (+ (* 2 (- n 1)) 1))
             k))


(tan-cf (/ pi 6) 10)
;Value: .5773519017263813

(tan (/ pi 6))
;Value: .5773519017263813

; exact!

