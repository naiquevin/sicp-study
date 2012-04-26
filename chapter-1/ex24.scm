;; Exercise 1.24
;; Modify the timed-prime-test procedure of exercise 1.22 to use fast-prime? (the
;; Fermat method), and test each of the 12 primes you found in that exercise. Since the Fermat test has
;; (log n) growth, how would you expect the time to test primes near 1,000,000 to compare with the time
;; needed to test primes near 1000? Do your data bear this out? Can you explain any discrepancy you find?

(load "scratch/fermat.scm")

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))
      #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  #t)

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


;; Output when times = 10

;; > (search-for-primes 1000 (+ 1000 50))

;; 1001
;; 1003
;; 1005
;; 1007
;; 1009 *** 213
;; 1011
;; 1013 *** 223
;; 1015
;; 1017
;; 1019 *** 234#f
;; > (search-for-primes 1000000 (+ 1000000 1000))

;; 1000001
;; 1000003 *** 415
;; 1000005
;; 1000007
;; 1000009
;; 1000011
;; 1000013
;; 1000015
;; 1000017
;; 1000019
;; 1000021
;; 1000023
;; 1000025
;; 1000027
;; 1000029
;; 1000031
;; 1000033 *** 389
;; 1000035
;; 1000037 *** 409#f

;; avg_1000 = 223, avg_1000000 = 377

;; output when times = 1000

;; > (search-for-primes 1000 (+ 1000 50))

;; 1001
;; 1003
;; 1005
;; 1007
;; 1009 *** 18201
;; 1011
;; 1013 *** 14255
;; 1015
;; 1017
;; 1019 *** 14713#f
;; > (search-for-primes 1000000 (+ 1000000 1000))

;; 1000001
;; 1000003 *** 45417
;; 1000005
;; 1000007
;; 1000009
;; 1000011
;; 1000013
;; 1000015
;; 1000017
;; 1000019
;; 1000021
;; 1000023
;; 1000025
;; 1000027
;; 1000029
;; 1000031
;; 1000033 *** 38308
;; 1000035
;; 1000037 *** 39747#f

;; avg_1000 = 15723, avg_100000 = 41157

;; since fermat's test has O(log n) growth, it means that 
;; the expected running time of 1000000 is 2 times that of 1000

;; When fermat is checked for 10 times the ratio is around 1.7 which can be called somewhat 
;; log(n)-ish growth.

;; But when its checked for 1000 times, the ratio rises to 2.61 which is unclear.


