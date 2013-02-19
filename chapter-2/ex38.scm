;; Exercise 2.38. The accumulate procedure is also known as
;; fold-right, because it combines the first element of the sequence
;; with the result of combining all the elements to the right. There
;; is also a fold- left, which is similar to fold-right, except that
;; it combines elements working in the opposite direction:

;; What are the values of

;; (fold-right / 1 (list 1 2 3))
;; (fold-left / 1 (list 1 2 3))
;; (fold-right list nil (list 1 2 3))
;; (fold-left list nil (list 1 2 3))

;; Give a property that op should satisfy to guarantee that fold-right
;; and fold-left will produce the same values for any sequence.


(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))


(fold-right / 1 (list 1 2 3))
;Value: 3/2

(fold-left / 1 (list 1 2 3))
;Value: 1/6

(define nil '())

(fold-right list nil (list 1 2 3))
;Value 11: (1 (2 (3 ())))

(fold-left list nil (list 1 2 3))
;Value 12: (((() 1) 2) 3)


;; So, the property that op must satisfy to guarantee that fold-left and
;; fold-right will produce the same values is the cummulative property
;; ie. (eq? (op a b) (op b a))

