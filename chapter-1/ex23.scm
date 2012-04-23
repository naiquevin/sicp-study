;; Exercise 1.23

(define (smallest-divisor n)
  (find-smallest-divisor n 2))

(define (divides? n a)
  (= 0 (remainder n a)))

(define (find-smallest-divisor n a)
  (cond ((> (square a) n) n)
        ((divides? n a) a)
        (else (find-smallest-divisor n (next a)))))

(define (next a)
  (if (= 2 a)
      3
      (+ a 2)))


(smallest-divisor 199)
;; 199

(smallest-divisor 1999)
;; 1999

(smallest-divisor 19999)
;; 7


;; Tried running the timed-prime-test using this version in DrRacket
;; and here are the results

;; > (search-for-primes 1000 (+ 1000 50))

;; 1001
;; 1003
;; 1005
;; 1007
;; 1009 *** 12
;; 1011
;; 1013 *** 12
;; 1015
;; 1017
;; 1019 *** 11#f
;; > (search-for-primes 10000 (+ 10000 100))
;;
;; 10001
;; 10003
;; 10005
;; 10007 *** 22
;; 10009 *** 19
;; 10011
;; 10013
;; 10015
;; 10017
;; 10019
;; 10021
;; 10023
;; 10025
;; 10027
;; 10029
;; 10031
;; 10033
;; 10035
;; 10037 *** 14
;; > (search-for-primes 100000 (+ 100000 200))
;;
;; 100001
;; 100003 *** 46
;; 100005
;; 100007
;; 100009
;; 100011
;; 100013
;; 100015
;; 100017
;; 100019 *** 45
;; 100021
;; 100023
;; 100025
;; 100027
;; 100029
;; 100031
;; 100033
;; 100035
;; 100037
;; 100039
;; 100041
;; 100043 *** 40#f

;; Observations:
;; the time differences fairly remained the same
;; as in exercise 22
;; Interestingly in some runs it showed an increase over the 
;; earlier version! May be because now we are using a user-defined
;; procedure instead of a primitive operation.

;; Frankly, I am not at all confident with these `timed` exercises
;; as I am still struggling to get consistent figures for the time
;; Will get back to these later..

