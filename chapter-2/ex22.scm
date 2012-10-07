;; Exercise 2.22. Louis Reasoner tries to rewrite the first
;; square-list procedure of exercise 2.21 so that it evolves an
;; iterative process:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things)) answer))))
  (iter items '()))


(square-list '(1 2 3 4 5 6))
;Value 17: (36 25 16 9 4 1)

;; Unfortunately, defining square-list this way produces the answer
;; list in the reverse order of the one desired. Why?

;; The answer is produced in the reverse order because, we start with
;; an empty list as the answer and in first iteration, cons the value
;; of square of the head (first element) of the input list. In the
;; second iteration the square of the second element is consed onto
;; the answer. In this way the squares of the later items in the list
;; get stacked upon the previous elements' in the answer. Therefore
;; after all iterations, the square of the last element will be first
;; and that of the first element will be last

;; Louis then tries to fix his bug by interchanging the arguments to
;; cons:

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer (square (car things))))))
  (iter items '()))

(square-list '(1 2 3 4 5 6))
;Value 18: ((((((() . 1) . 4) . 9) . 16) . 25) . 36)

;; This solution after interchanging the sequence of args to cons
;; doesn't work too because in this case, in every iteration the value
;; of a list is consed onto an atom.

;; A list is a link of pairs such that the first element of every pair
;; points to an atom and the 2nd element points to a similar such pair
;; In this case however, the first element points to a pair and 2nd
;; element is an atom.

