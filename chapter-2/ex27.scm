;; Exercise 2.27. Modify your reverse procedure of exercise 2.18 to
;; produce a deep-reverse procedure that takes a list as argument and
;; returns as its value the list with its elements reversed and with
;; all sublists deep-reversed as well. For example,

;; (define x (list (list 1 2) (list 3 4)))

;; x
;; ((1 2) (3 4))
;; (reverse x)
;; ((3 4) (1 2))
;; (deep-reverse x)
;; ((4 3) (2 1))

;; normal reverse

(define (reverse x)
  (define (iter x result)
    (if (null? x)
        result
        (iter (cdr x) (cons (car x) result))))
  (iter x '()))


(define x (list (list 1 2) (list 3 4)))

(reverse x)
;Value 11: ((3 4) (1 2))


;; deep-reverse

(define (deep-reverse x)
  (define (iter x result)
    (cond ((null? x) result)
          ((pair? (car x))
           (iter (cdr x) (cons (deep-reverse (car x)) result)))
          (else
           (iter (cdr x) (cons (car x) result)))))
  (iter x '()))

(deep-reverse x)
;Value 12: ((4 3) (2 1))
