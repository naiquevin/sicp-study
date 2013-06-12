;; Exercise 2.49. Use segments->painter to define the following
;; primitive painters:

;; a. The painter that draws the outline of the designated frame.

;; b. The painter that draws an ``X'' by connecting opposite corners
;; of the frame.

;; c. The painter that draws a diamond shape by connecting the
;;    midpoints of the sides of the frame.

;; d. The wave painter.

#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(provide outline
         cross
         diamond
         wave)


(define outline
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 1 0))
         (make-segment (make-vect 1 0) (make-vect 1 1))
         (make-segment (make-vect 0 1) (make-vect 1 1))
         (make-segment (make-vect 0 0) (make-vect 0 1)))))


(define cross
  (segments->painter
   (list (make-segment (make-vect 0 0) (make-vect 1 1))
         (make-segment (make-vect 1 0) (make-vect 0 1)))))


(define diamond
  (segments->painter
   (list (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
         (make-segment (make-vect 0.5 1) (make-vect 1 0.5))
         (make-segment (make-vect 1 0.5) (make-vect 0.5 0))
         (make-segment (make-vect 0.5 0) (make-vect 0 0.5)))))


(define wave
  (segments->painter
   (list (make-segment (make-vect 0.4 1) (make-vect 0.3 0.85))
         (make-segment (make-vect 0.3 0.85) (make-vect 0.4 0.7))
         (make-segment (make-vect 0.4 0.7) (make-vect 0.25 0.7))
         (make-segment (make-vect 0.25 0.7) (make-vect 0.1 0.65))
         (make-segment (make-vect 0.1 0.65) (make-vect 0 0.85))
         (make-segment (make-vect 0 0.7) (make-vect 0.1 0.5))
         (make-segment (make-vect 0.1 0.5) (make-vect 0.25 0.6))
         (make-segment (make-vect 0.25 0.6) (make-vect 0.35 0.5))
         (make-segment (make-vect 0.35 0.5) (make-vect 0.2 0))
         (make-segment (make-vect 0.35 0) (make-vect 0.5 0.4))
         (make-segment (make-vect 0.5 0.4) (make-vect 0.65 0))
         (make-segment (make-vect 0.8 0) (make-vect 0.65 0.5))
         (make-segment (make-vect 0.65 0.5) (make-vect 1 0.3))
         (make-segment (make-vect 1 0.4) (make-vect 0.75 0.7))
         (make-segment (make-vect 0.75 0.7) (make-vect 0.6 0.7))
         (make-segment (make-vect 0.6 0.7) (make-vect 0.7 0.85))
         (make-segment (make-vect 0.7 0.85) (make-vect 0.6 1)))))

