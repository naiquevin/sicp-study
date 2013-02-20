;; Exercise 2.39. Complete the following definitions of reverse
;; (exercise 2.18) in terms of fold-right and fold-left from exercise
;; 2.38:

;; (define (reverse sequence)
;;   (fold-right (lambda (x y) <??>) nil sequence))
;; (define (reverse sequence)
;;   (fold-left (lambda (x y) <??>) nil sequence))


(load "ex38.scm")

(define (reverse sequence)
  (fold-right 
   (lambda (x y) 
     (append y (list x))) ;; y <- reduction and x <- new item
   '()
   sequence))

(reverse '(1 2 3))


(define (reverse sequence)
  (fold-left 
   (lambda (x y)
     (cons y x)) ;; x <- reduction and y <- new item
   '()
   sequence))

(reverse '(1 2 3))

