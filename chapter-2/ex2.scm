;; Segments and points

;; Exercise 2.2. Consider the problem of representing line segments in a
;; plane. Each segment is represented as a pair of points: a starting
;; point and an ending point. Define a constructor make-segment and
;; selectors start-segment and end-segment that define the representation
;; of segments in terms of points. Furthermore, a point can be
;; represented as a pair of numbers: the x coordinate and the y
;; coordinate.  Accordingly, specify a constructor make-point and
;; selectors x-point and y-point that define this
;; representation. Finally, using your selectors and constructors, define
;; a procedure midpoint-segment that takes a line segment as argument and
;; returns its midpoint (the point whose coordinates are the average of
;; the coordinates of the endpoints). To try your procedures, you'll need
;; a way to print points:

(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))


;; point
(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))


(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display (y-point p))
  (display ")"))


;; Midpoint of a segment

(define (average a b)
  (/ (+ a b) 2))

(define (midpoint-segment segment)
  (let ((point-start (start-segment segment))
        (point-end (end-segment segment)))
    (let ((start-x (x-point point-start))
          (start-y (y-point point-start))
          (end-x (x-point point-end))
          (end-y (y-point point-end)))
      (make-point (average start-x end-x)
                  (average start-y end-y)))))


(define point1 (make-point 0 0))
(define point2 (make-point 10 10))
(define seg1 (make-segment point1 point2))
(print-point (midpoint-segment seg1))


(define point3 (make-point -4 -4))
(define point4 (make-point 4 4))
(define seg2 (make-segment point3 point4))
(print-point (midpoint-segment seg2))

