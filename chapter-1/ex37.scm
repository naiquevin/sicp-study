
(define (cont-frac nfunc dfunc k)
  (define (cont-frac-recur i)
    (if (= i k)
        (/ (nfunc i) (dfunc i)) ; actually (/ (nfunc i) (+ (dfunc i) 0))
        (/ (nfunc i)
           (+ (dfunc i)
              (cont-frac-recur (+ i 1))))))
  (cont-frac-recur 1))
    

(define (golden-inverse k func)  
  (func (lambda (i) 1.0)
        (lambda (i) 1.0)
        k))

;; phi val obtained from the google calculator
(/ 1 1.61803399)
;Value: .6180339882723972

(golden-inverse 11 cont-frac)
;Value: .6180555555555556

;; Value of k to get accuracy to 4 dec places = 10

;; Iterative process

(define (cont-frac-i nfunc dfunc k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (nfunc i)
                 (+ (dfunc i)
                    result)))))
  (iter k 0))


;; result = 0 at the start of iteration because when we truncate
;; the expansion at k, we are infact assuming that the value
;; of cont-frac for k+1 = 0
;; (Remember! for iterative process, start with known and iter from next)
;; refer to the comment in recursive process above to understand 
;; this better

(golden-inverse 11 cont-frac-i)
;Value: .6180555555555556

