;; Exercise 2.33. Fill in the missing expressions to complete the
;; following definitions of some basic list- manipulation operations
;; as accumulations:

;; (define (map p sequence)
;;   (accumulate (lambda (x y) <??>) nil sequence))

;; (define (append seq1 seq2)
;;   (accumulate cons <??> <??>))

;; (define (length sequence)
;;   (accumulate <??> 0 sequence))


(load "scratch/composition.scm")

(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              '()
              sequence))

(map (lambda (x) (square x)) '(1 2 3 4 5 6))
;Value 15: (1 4 9 16 25 36)


(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(append '(1 2 3 4) '(5 6 7 8))
;Value 16: (1 2 3 4 5 6 7 8)


(define (length seq)
  (accumulate (lambda (x y)
                (+ y 1))
              0
              seq))


(length '(1 2 3 4 5))
;Value: 5
                      
