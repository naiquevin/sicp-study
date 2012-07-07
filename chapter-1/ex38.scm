;; In 1737, the Swiss mathematician Leonhard Euler published a memoir
;; De Fractionibus Continuis, which included a continued fraction
;; expansion for e - 2, where e is the base of the natural
;; logarithms. In this fraction, the Ni are all 1, and the Di are
;; successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ....  Write a program
;; that uses your cont-frac procedure from exercise 1.37 to
;; approximate e, based on Euler's expansion.


;; iterative solution to cont-frac from ex1.37

(define (cont-frac nfunc dfunc k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (nfunc i)
                 (+ (dfunc i)
                    result)))))
  (iter k 0))

;; deducing the function Di

;; 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ....

;; The pattern is that after 2 every 3rd element is a successive
;; multiple of 2. Other elements are 1

;; elements of the form n = 2 + 3x where x is integer are
;; non ones and equal to 2 * (x + 1)

;; so f(n) = 2 * ((n - 2)/3 + 1), if (n - 2) % 3 == 0
;;         = 1 otherwise


(define (dofi i)
  (let ((j (- i 2)))
    (if (= 0 (remainder j 3))
        (* 2 (+ 1 (/ j 3)))
        1)))


(define (nlog-base k)
  (+ 2 (cont-frac (lambda (i) 1.0)
                  dofi
                  k)))

;; Value of e from Wikipedia = 2.718281828

(define e (nlog-base 5))
e
;Value: 2.71875

(define e (nlog-base 7))
e
;Value: 2.7183098591549295

