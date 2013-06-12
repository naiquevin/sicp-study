#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(provide right-split
         square-of-four)

;; (paint einstein)

(define (right-split painter n)
  (if (= 0 n)
      painter
      (let ([smaller (right-split painter (- n 1))])
        (beside painter (below smaller smaller)))))


(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ([top (beside (tl painter) (tr painter))]
          [bottom (beside (bl painter) (br painter))])
      (below bottom top))))
