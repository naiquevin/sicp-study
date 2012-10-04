;; Define a procedure reverse that takes a list as argument and
;; returns a list of the same elements in reverse order:

;; (reverse (list 1 4 9 16 25))
;; (25 16 9 4 1)

(define (reverse lat)
  (define (reverse-iter lat result)
    (if (null? lat)
        result
        (reverse-iter (cdr lat) (cons (car lat) result))))
  (reverse-iter lat '()))
  
(reverse (list 1 4 9 16 25))

;; Another approach
(define (reverse2 lat)
  (cond ((= (length lat) 1) lat)
        ((= (length lat) 2) (cons (car (cdr lat)) (list (car lat))))
        (else (append (reverse2 (cdr lat)) (list (car lat))))))

(reverse2 (list 1 4 9 16 25))
      

