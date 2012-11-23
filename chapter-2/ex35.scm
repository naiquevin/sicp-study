;; Exercise 2.35. Redefine count-leaves from section 2.2.2 as an
;; accumulation:

;; (define (count-leaves t)
;;   (accumulate <??> <??> (map <??> <??>)))

(load "scratch/composition.scm")


(define (count-leaves-old t)
  (cond ((null? t) 0)
        ((not (pair? t)) 1)
        (else 
         (+ (count-leaves-old (car t))
            (count-leaves-old (cdr t))))))


(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x)
                     (if (not (pair? x))
                         1
                         (count-leaves x)))
                   t)))

(define t1 (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(count-leaves-old t1)
;Value: 7

(count-leaves t1)
;Value: 7

