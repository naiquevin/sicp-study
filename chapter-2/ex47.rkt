;; Exercise 2.47. Here are two possible constructors for frames:

;; (define (make-frame origin edge1 edge2)
;;   (list origin edge1 edge2))

;; (define (make-frame origin edge1 edge2)
;;   (cons origin (cons edge1 edge2)))

#lang racket

(require "ex46.rkt")

(provide frame-coord-map
         make-frame
         origin-frame
         edge1-frame
         edge2-frame)

;; implementation of the frame-coordinate map

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))


;; Selectors for 1st implementation of `make-frame`

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1-frame frame)
  (cadr frame))

(define (edge2-frame frame)
  (last frame))


;; Selectors for 2nd implementation of `make-frame`

(define (make-frame2 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))


(define (origin-frame2 frame)
  (car frame))

(define (edge1-frame2 frame)
  (car (cdr frame)))

(define (edge2-frame2 frame)
  (cdr (cdr frame)))

