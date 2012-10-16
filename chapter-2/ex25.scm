;; Exercise 2.25. Give combinations of cars and cdrs that will pick 7
;; from each of the following lists:

;; (1 3 (5 7) 9)
;; ((7))
;; (1 (2 (3 (4 (5 (6 7))))))

(define a '(1 3 (5 7) 9))
(define b '((7)))
(define c '(1 (2 (3 (4 (5 (6 7)))))))

(= 7 (car (cdr (car (cdr (cdr a))))))
;Value: #t

(= 7 (car (car b)))
;Value: #t

(= 7 (car (cdr 
           (car (cdr 
                 (car (cdr 
                       (car (cdr 
                             (car (cdr 
                                   (car (cdr c)))))))))))))
;Value: #t
