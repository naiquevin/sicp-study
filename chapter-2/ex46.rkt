;; Exercise 2.46. A two-dimensional vector v running from the origin
;; to a point can be represented as a pair consisting of an
;; x-coordinate and a y-coordinate. Implement a data abstraction for
;; vectors by giving a constructor make-vect and corresponding
;; selectors xcor-vect and ycor-vect. In terms of your selectors and
;; constructor, implement procedures add-vect, sub-vect, and
;; scale-vect that perform the operations vector addition, vector
;; subtraction, and multiplying a vector by a scalar:

;; (x1, y1) + (x2, y2) = (x1 + x2, y1 + y2)
;; (x1, y1) - (x2, y2) = (x1 - x2, y1 - y2)
;; s . (x, y)          = (s.x + s.y)

#lang racket

(provide make-vect
         xcor-vect
         ycor-vect
         add-vect
         sub-vect
         scale-vect)


(define (make-vect x y)
  (cons x y))

(define (xcor-vect vector)
  (car vector))

(define (ycor-vect vector)
  (cdr vector))


(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1)
                (xcor-vect v2))
             (+ (ycor-vect v1)
                (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1)
                (xcor-vect v2))
             (- (ycor-vect v1)
                (ycor-vect v2))))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))


;; unittests

(module+ test
  (require rackunit)
  
  (define v1 (make-vect 5 7))
  (define v2 (make-vect 1 2))
    
  (check-equal? v2 '(1 . 2) "Vector constructor")
  (check-equal? (xcor-vect v2) 1 "x-cord selector for vector")
  (check-equal? (ycor-vect v2) 2 "x-cord selector for vector")
  (check-equal? (add-vect v1 v2) '(6 . 9) "Vector addition")
  (check-equal? (sub-vect v1 v2) '(4 . 5) "Vector subtraction")
  (check-equal? (scale-vect 5 v2) '(5 . 10) "Vector scaling/multiplication")
    
  )

