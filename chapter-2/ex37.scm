;; Exercise 2.37. Matrix operations


(load "scratch/composition.scm")


(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map (lambda (x)
                                       (car x))
                                     seqs))
            (accumulate-n op init (map (lambda (x)
                                         (cdr x))
                                       seqs)))))


;; Some example matrices and vectors for testing
(define m1 (list '(1 2 3) '(4 5 6) '(7 8 9)))
(define m2 (list '(2 3) '(4 5) '(6 7)))
(define v1 '(1 2 3))


(define (matrix-*-vector m v)
  (map (lambda (r)
         (accumulate + 0 (accumulate-n * 1 (list r v))))
       m))

(matrix-*-vector m1 v1)
;Value 9: (14 32 50)


(define (transpose mat)
  (accumulate-n cons '() mat))


(transpose m1)
;Value 12: ((1 4 7) (2 5 8) (3 6 9))

(transpose m2)
;Value 13: ((2 4 6) (3 5 7))

(transpose '((1 2 3)))
;Value 15: ((1) (2) (3))


(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r)
           (map (lambda (c)
                  (accumulate + 0 (accumulate-n * 1 (list r c))))
                cols))
         m)))

(matrix-*-matrix m1 m2)
;Value 18: ((28 34) (64 79) (100 124))

