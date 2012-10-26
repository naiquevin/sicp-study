;; Counting leaves

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else 
         (+ (count-leaves (car x))
            (count-leaves (cdr x))))))

;; Mapping over trees

(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else
         (cons (scale-tree (car tree) factor)
               (scale-tree (cdr tree) factor)))))

(define t1 (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(scale-tree t1 10)


;; using map

(define (scale-tree tree factor)
  (map (lambda (subtree)
         (if (pair? subtree)
             (scale-tree subtree factor)
             (* subtree factor)))
       tree))

(scale-tree t1 10)

