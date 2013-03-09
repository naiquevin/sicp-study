;; Nested Mappings

(load "scratch/composition.scm") ; accumulate
(load "../chapter-1/ex21.scm") ; smallest-divisor
(load "../chapter-1/ex22.scm") ; prime?


(define (enumerate-interval i j)
  (define (iter i j result)
    (if (< j i)
        result
        (iter i (- j 1) (cons j result))))
  (iter i j '()))


(accumulate append
            '()
            (map (lambda (i)
                   (map (lambda (j)
                          (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 6)))


;; Generic procedure 
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))


(flatmap (lambda (i)
           (map (lambda (j)
                  (list i j))
                (enumerate-interval 1 (- i 1))))
         (enumerate-interval 1 6))


(define (prime-sum? pair)
  (prime? (+ (car pair) 
             (cadr pair))))


(filter prime-sum?
        (flatmap (lambda (i)
                   (map (lambda (j)
                          (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 6)))


(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap (lambda (i)
                          (map (lambda (j)
                                 (list i j))
                               (enumerate-interval 1 (- i 1))))
                        (enumerate-interval 1 n)))))

(prime-sum-pairs 6)
;Value 29: ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))


;; permutations

(define (permultations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p)
                        (cons x p))
                      (permultations (remove x s))))
               s)))

(define (remove item seq)
  (filter (lambda (x)
            (not (eq? item x)))
          seq))

(permultations '(1 2 3))      

