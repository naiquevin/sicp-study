
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))


(accumulate + 0 '(1 2 3 4 5))
(accumulate cons 
            '() 
            '(1 2 3 4 5))


