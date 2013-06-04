;; Exercise 2.50. Define the transformation flip-horiz, which flips
;; painters horizontally, and transformations that rotate painters
;; counterclockwise by 180 degrees and 270 degrees.

#lang racket

(require (except-in (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1))
                    flip-horiz))

(provide rotate-270)

;; Note: The transform-painter implementation in sicp.plt is different
;; from the one discussed in the book in that instead of taking 4 args
;; as input, it takes only 3 args and returns a procedure as an output
;; that takes the painter
;; 
;; Credit of this goes to billthelizard

(define (flip-horiz painter)
  ((transform-painter (make-vect 1.0 0.0)
                      (make-vect 0.0 0.0)
                      (make-vect 1.0 1.0))
   painter))


(define (rotate-180 painter)
  ((transform-painter (make-vect 1.0 1.0)
                      (make-vect 0.0 1.0)
                      (make-vect 1.0 0.0))
   painter))


(define (rotate-270 painter)
  ((transform-painter (make-vect 1.0 0.0)
                      (make-vect 0.0 0.0)
                      (make-vect 1.0 1.0))
   painter))
