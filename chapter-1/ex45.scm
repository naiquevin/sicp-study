
;; Multiple average damping

(load "scratch/return_procs.scm")

;; load ex42 and ex43 before running this
(load "ex42.scm") ; compose
(load "ex43.scm") ; repeated


(define (nthpower x n)
  (define (iter c result)
    (if (= 0 c)
        result
        (iter (- c 1) 
              (* result x))))
  (iter n 1))


(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

(sqrt 25)
;Value: 5.000000000053722


(define (cuberoot x)
  (fixed-point-of-transform (lambda (y) (/ x (nthpower y 2)))
                            average-damp
                            1.0))

(cuberoot 27)
;Value: 3.000022143521597


(define (fourthroot x)
  (fixed-point-of-transform (lambda (y) (/ x (nthpower y 3)))
                            (repeated average-damp 2)
                            1.0))

;; fourthroot requires average-damp to be applied twice

(fourthroot 16)
;Value: 2.0000000000021965


(define (fifthroot x)
  (fixed-point-of-transform (lambda (y) (/ x (nthpower y 4)))
                            (repeated average-damp 2)
                            1.0))

(fifthroot 32)
;Value: 1.9999939482000788


;; Trying eigthroot now

(define (eigthroot x)
  (fixed-point-of-transform (lambda (y) (/ x (nthpower y 7)))
                            (repeated average-damp 3)
                            1.0))

(eigthroot 256)
;Value: 2.0000000000039666

(define (seventh x)
  (fixed-point-of-transform (lambda (y) (/ x (nthpower y 6)))
                            (repeated average-damp 2)
                            1.0))

(seventh 128)
;Value: 2.000035500809081

;; Observed Pattern

;; < 4 -> 1
;; > 4 -> 2
;; > 8 -> 3
;; > n -> (floor (log-2 n))

(define (log-to-the-base n)
  (lambda (x)
    (/ (log x) (log n))))


(define (log-2 x)
  ((log-to-the-base 2) x))


(define (nthroot x n)
  (define repeat-times (floor (log-2 n)))
  (fixed-point-of-transform (lambda (y) (/ x (nthpower y (- n 1))))
                            (repeated average-damp repeat-times)
                            1.0))

(nthroot 2 2)
(nthroot 4 2)
(nthroot 9 2)
(nthroot 1024 10)
(nthroot 729 2)
(nthroot 729 3)

