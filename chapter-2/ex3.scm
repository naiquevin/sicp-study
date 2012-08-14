
;; Rectangles in a plane

;; Exercise 2.3. Implement a representation for rectangles in a
;; plane. (Hint: You may want to make use of exercise 2.2.) In terms of
;; your constructors and selectors, create procedures that compute the
;; perimeter and the area of a given rectangle. Now implement a different
;; representation for rectangles. Can you design your system with
;; suitable abstraction barriers, so that the same perimeter and area
;; procedures will work using either representation?

(load "ex2.scm")


(define (square x)
  (* x x))


;; length of a segment
(define (length-segment segment)
  (let ((s (start-segment segment))
        (e (end-segment segment)))
    (let ((sx (x-point s))
          (sy (y-point s))
          (ex (x-point e))
          (ey (y-point e)))
      (sqrt (+ (square (- ex sx))
               (square (- ey sy)))))))


;; rectange from four points represented as a pair of it's length and
;; breadth segments
(define (make-rectangle p1 p2 p3 p4)
  (let ((s1 (make-segment p1 p2))
        (s2 (make-segment p2 p3))
        (s3 (make-segment p1 p3)))
    (lengh-breadth-pair s1 s2 s3)))

;; get length and breadth pair from 3 sides
;; length, breadth and diagonal
(define (lengh-breadth-pair s1 s2 s3)
  (let ((l1 (length-segment s1))
        (l2 (length-segment s2))
        (l3 (length-segment s3)))
    (cond ((and (> l1 l2) (> l1 l3)) 
           (cons (max-segment s2 s3) (min-segment s2 s3)))
          ((and (> l2 l1) (> l2 l3))
           (cons (max-segment s1 s3) (min-segment s1 s3)))
          (else
           (cons (max-segment s1 s2) (min-segment s1 s2))))))

;; get the max of two segments
(define (max-segment s1 s2)
  (if (> (length-segment s1) (length-segment s2))
      s1
      s2))

;; get the min of two segments
(define (min-segment s1 s2)
  (if (< (length-segment s1) (length-segment s2))
      s1
      s2))


;; selectors
(define (rect-length rect)
  (length-segment (car rect)))

(define (rect-breadth rect)
  (length-segment (cdr rect)))


(define (perimeter rectangle)
  (let ((l (rect-length rectangle))
        (b (rect-breadth rectangle)))
    (* 2 (+ l b))))


(define (area rectangle)
  (let ((l (rect-length rectangle))
        (b (rect-breadth rectangle)))
    (* l b)))


(define p1 (make-point 0 0))
(define p2 (make-point 4 0))
(define p3 (make-point 4 3))
(define p4 (make-point 3 0))

;; (make-segment p1 p2)
;; (length-segment (make-segment p1 p4))

(define rectangle (make-rectangle p1 p2 p3 p4))

(perimeter rectangle)
(area rectangle)


;; Representing a rectangle as diagonal and angle between the diagonal
;; and length segment

;; constructor
(define (make-rectangle p1 p2 t1)
  (cons (make-segment p1 p2) t1))

;; selectors
(define (rect-length rectangle)
  (* (length-segment (car rectangle)) (cos (cdr rectangle))))

(define (rect-breadth rectangle)
  (* (length-segment (car rectangle)) (sin (cdr rectangle))))


(define rectangle (make-rectangle p1 p3 20))

(perimeter rectangle)
(area rectangle)

