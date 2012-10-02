;; Function definitions for List operation

;; This is just for the sake of exercise and understanding. These are
;; implemented as builtin compund procedures

(cons 1 (cons 2 (cons 3 (cons 4 '()))))
;; is the same as 
(list 1 2 3 4)
;; is the same as 
'(1 2 3 4)

(define somelist (list 3 4 5 7 7 8))

;; list-reference procedure
(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(list-ref somelist 2)

;; length of a list (recursive)
(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(length somelist)

(define odds (list 1 3 5 7 9))

(define evens (list 2 4 6 8))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(append somelist odds)
      
            
