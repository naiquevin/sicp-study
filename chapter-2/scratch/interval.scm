
;; procedure to print an interval

(define (print-interval x)
  (newline)
  (display (lower-bound x))
  (display "-")
  (display (upper-bound x))
  (newline)
  x)
