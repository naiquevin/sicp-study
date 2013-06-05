;; Exercise 2.51. Define the below operation for painters. Below takes
;; two painters as arguments. The resulting painter, given a frame,
;; draws with the first painter in the bottom of the frame and with
;; the second painter in the top. Define below in two different ways
;; -- first by writing a procedure that is analogous to the beside
;; procedure given above, and again in terms of beside and suitable
;; rotation operations (from exercise 2.50).

#lang racket

(require (except-in (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1))
                    below))

(require "ex50.rkt")


;; implementation of `below` similar to the one of `beside`

(define (below painter1 painter2)
  (let ([split-point (make-vect 0.0 0.5)])
    (let ([paint-bottom
           ((transform-painter (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               (make-vect 0.0 0.5))
            painter1)]
          [paint-top
           ((transform-painter (make-vect 0.0 0.5)
                               (make-vect 1.0 0.5)
                               (make-vect 0.0 1.0))
            painter2)])
      (lambda (frame)
        (paint-top frame)
        (paint-bottom frame)))))


;; implementation of below using rotate transforms and beside

;; Note: The implementation of rotate90 in sicp.plt seems to be
;; different from the ones discussed in the book due to which the
;; rotate* impls from sicp.plt with rotate-* impls in exercise
;; solutions don't compose well together. Hence defining a `rotate-90`
;; here

(define (rotate-90 painter)
  ((transform-painter (make-vect 0.0 1.0)
                      (make-vect 0.0 0.0)
                      (make-vect 1.0 1.0))
  painter))

(define (below2 painter1 painter2)
  (rotate-270 (beside (rotate-90 painter1)
                      (rotate-90 painter2))))

