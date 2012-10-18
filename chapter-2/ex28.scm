;; Exercise 2.28. Write a procedure fringe that takes as argument a
;; tree (represented as a list) and returns a list whose elements are
;; all the leaves of the tree arranged in left-to-right order. For
;; example,

;; (define x (list (list 1 2) (list 3 4)))
;; (fringe x)
;; (1 2 3 4)
;; (fringe (list x x))
;; (1 2 3 4 1 2 3 4)

(define (fringe x)
  (define (iter x result)
    (cond ((null? x) result)
          ((not (pair? (car x)))
           (iter (cdr x) (append result (list (car x)))))
          (else
           (iter (cdr x) (append result (fringe (car x)))))))
  (iter x '()))


(define x (list (list 1 2) (list 3 4)))

(fringe x)
;Value 17: (1 2 3 4)

(fringe (list x x))
;Value 18: (1 2 3 4 1 2 3 4)
          
