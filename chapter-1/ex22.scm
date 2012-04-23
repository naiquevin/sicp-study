;; timed-prime-test
;; I haven't been able to find an alternative for 
;; runtime in MIT scheme. And there's doesn't seem to
;; be current-inexact-milliseconds as well.
;; So, I am not able to proceed with this exercise, and thus,
;; leaving it incomplete for now and would come back to it later.

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))


(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      #f))


(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  #t)

;; smallest-divisor procedure is defined in ex21.scm
(define (prime? n)
  (= n (smallest-divisor n)))


(define (search-for-primes range-start range-end)
  (iter-range (odd-range-start range-start) range-end 0))


(define (iter-range num range-end tot)
  (cond ((and (< tot 3)
              (timed-prime-test num)
              (< (+ num 2) range-end))
         (iter-range (+ 2 num) range-end (+ 1 tot)))
         ((and (< tot 3)
               (< (+ num 2) range-end))
          (iter-range (+ 2 num) range-end tot))
         (else #f)))


(define (odd-range-start range-start)
  (if (even? range-start)
      (+ 1 range-start)
      (range-start)))


(search-for-primes 1000 (+ 1000 50))
(search-for-primes 10000 (+ 10000 100))
(search-for-primes 100000 (+ 100000 200))



