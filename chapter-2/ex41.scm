;; Exercise 2.41. Write a procedure to find all ordered triples of
;; distinct positive integers i, j, and k less than or equal to a
;; given integer n that sum to a given integer s.

;; load reusable procedures from other file
(load "scratch/nested_mappings.scm")


(define (ordered-triplets n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))


(ordered-triplets 4)
;Value 18: ((3 2 1) (4 2 1) (4 3 1) (4 3 2))


(define (sum seq)
  (fold-right + 0 seq))


(define (sum-ordered-triplets n s)
  (filter (lambda (triplet)
            (= s (sum triplet)))
          (ordered-triplets n)))


(sum-ordered-triplets 10 10)
;Value 23: ((5 3 2) (5 4 1) (6 3 1) (7 2 1))


;; Original recursive solution by first finding combinations
;; Turned out to be highly inefficient
;; Need to find out a faster way to compute combinations

(define (combinations seq r)
  (cond ((= r 0) (list '()))
        ((= r (length seq)) (list seq))
        (else
         (flatmap (lambda (x)
                    (let ((withoutx (remove x seq)))
                      (append
                       (combinations withoutx r)
                       (let ((rc (combinations withoutx (- r 1))))
                         (if (= (length rc) 0)
                             (list x)
                             (map (lambda (y)
                                    (cons x y))
                                  rc))))))
                  seq))))

;; (combinations '(1 2) 1)
;; (combinations '(1 2 3) 2)
;; (combinations '(1 2 3) 1)
;; (combinations (enumerate-interval 1 7) 3)


(define (sort-asc seq)
  (sort seq (lambda (x y)
              (< x y))))


(define (unique seq)
  (fold-right (lambda (x y)
                (if (equal? (member x y) #f)
                    (cons x y)
                    y))
             '()
             seq))

;; (unique '(1 2 3 1 2))
;; (unique '((1 2) (1 2) (2 3)))


(define (ordered-triplets n s)
  (filter (lambda (x)
            (= (sum x) s))
          (unique (map sort-asc
                       (combinations
                        (enumerate-interval 1 n) 3)))))

;; (ordered-triplets 6 6)
;Value 21: ((1 2 3))

;; works but highly inefficient solution
;; runs out of memory for larger sequences

