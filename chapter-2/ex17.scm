;; Exercise 2.17. Define a procedure last-pair that returns the list
;; that contains only the last element of a given (nonempty) list:
;; (last-pair (list 23 72 149 34)) (34)

(define (last-pair items)
  (let ((tail (cdr items)))
    (if (null? tail)
        items
        (last-pair tail))))


(last-pair (list 23 72 149 34))
             
