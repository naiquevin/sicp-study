;; Exercise 2.52. Make changes to the square limit of wave shown in
;; figure 2.9 by working at each of the levels described above. In
;; particular:

;; a. Add some segments to the primitive wave painter of exercise 2.49
;; (to add a smile, for example).

;; b. Change the pattern constructed by corner-split (for example, by
;; using only one copy of the up- split and right-split images instead
;; of two).

;; c. Modify the version of square-limit that uses square-of-four so
;; as to assemble the corners in a different pattern. (For example,
;; you might make the big Mr. Rogers look outward from each corner of
;; the square.)

#lang racket

(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(require "ex49.rkt"
         "ex44.rkt"
         "scratch/picture.rkt")


;; a. wave with a smiley

(define wave-smile
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
         (make-segment (make-vect 0.7 0.85) (make-vect 0.6 1))
         (make-segment (make-vect 0.45 0.8) (make-vect 0.5 0.75))
         (make-segment (make-vect 0.5 0.75) (make-vect 0.55 0.8)))))


;; b. corner-split to use only 1 copy of up-split and right-split

(define (corner-split painter n)
  (if (= n 0)
      painter
      (let ([up (up-split painter (- n 1))]
            [right (right-split painter (- n 1))])
        (let ([top-left up]
              [bottom-right right]
              [corner (corner-split painter (- n 1))])
          (beside (below painter top-left)
                  (below bottom-right corner))))))

;; c. painter that paints Mr. Rogers looking outward from each corner

(define (square-limit painter n)
  (let ([combine4 (square-of-four flip-horiz identity
                                  flip-horiz identity)])
    (combine4 (corner-split painter n))))
