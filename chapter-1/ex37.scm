
(define (cont-frac nfunc dfunc k)
  (define (cont-frac-recur i)
    (if (= i k)
        (/ (nfunc i) (dfunc i))
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

(golden-inverse 10 cont-frac)
;Value: .6179775280898876

(golden-inverse 11 cont-frac)
;Value: .6180555555555556

;; Value of k to get accuracy to 4 dec places = 11

(define (cont-frac-i nfunc dfunc k)
  (define (iter i result)
    (if (= i 1)
        result
        (iter (- i 1)
              (/ (nfunc i)
                 (+ (dfunc i)
                    result)))))
  (iter k 1.0))

(golden-inverse 11 cont-frac-i)
;Value: .6180555555555556

