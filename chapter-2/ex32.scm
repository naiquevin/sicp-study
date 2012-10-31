;; Exercise 2.32. We can represent a set as a list of distinct
;; elements, and we can represent the set of all subsets of the set as
;; a list of lists. For example, if the set is (1 2 3), then the set
;; of all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2
;; 3)). Complete the following definition of a procedure that
;; generates the set of subsets of a set and give a clear explanation
;; of why it works:

;; (define (subsets s)
;;   (if (null? s)
;;       (list nil)
;;       (let ((rest (subsets (cdr s))))
;;         (append rest (map <??> rest)))))


(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x)
                            (cons (car s) x))
                          rest)))))


;; This procedure processes the list by dividing it in two parts as follows

;; 1. Get tail of the list (rest) ie. without the first element
;; 2. Add head (first element) to every element in tail using map
;; 3. subset of an empty list is nil or '() 
;; 4. Return union of 1 and 2 by appending 

;; The recursive process is generated as follows

;; (subsets '(1 2 3)) <- s = '(1 2 3)

;; '(1 2 3)
;; rest -> (subsets '(2 3)) <- s = '(2 3)
;;         ((2) (3) (2 3) ())

;; '(2 3)
;; rest -> (subsets '(3)) <- s = '(3)
;;         ((3) ())

;; '()
;; '(())

;; (append (()) ((3))) <- s = '(3)
;; (() (3))

;; (append (() (3)) ((2) (2 3))) <- s = '(2 3)
;; (() (3) (2) (2 3))

;; (append (() (3) (2) (2 3)) ((1) (1 3) (2 3) (1 2 3))) <-  = '(1 2 3)
;; (() (3) (2) (2 3) (1) (1 3) (2 3) (1 2 3))

