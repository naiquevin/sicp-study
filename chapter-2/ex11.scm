
(load "ex7.scm")

;; The old interval multiplier procedure
(define (mul-interval-old x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


;; 9 Cases
;; ====================================================================
;; x1  x2  y1  y2  | p1  p2  p3  p4  |
;; ====================================================================
;; +ve +ve +ve +ve | +ve +ve +ve +ve | (make-interval
;;                 |                 |   (* (min x1 x2) (min y1 y2))
;;                 |                 |   (* (max x1 x2) (max y1 y2)))
;; --------------------------------------------------------------------
;; +ve +ve -ve +ve | -ve +ve -ve +ve | (make-interval
;;                 |                 |   (* (max x1 x2) y1)
;;                 |                 |   (* (max x1 x2) y2)
;; --------------------------------------------------------------------
;; +ve +ve -ve -ve | -ve -ve -ve -ve | (make-interval
;;                 |                 |  (* (max x1 x2)
;;                 |                 |     (min y1 y2))
;;                 |                 |  (* (min x1 x2)
;;                 |                 |     (max y1 y2)))
;; --------------------------------------------------------------------
;; -ve +ve +ve +ve | -ve -ve +ve +ve | (make-interval
;;                 |                 |  (* x1 (max y1 y2))
;;                 |                 |  (* x2 (max y1 y2)))
;; --------------------------------------------------------------------
;; -ve +ve -ve +ve | +ve -ve -ve +ve | (make-interval
;;                 |                 |  (min (* x1 y2) (* x2 y1))      <---- more than 2 mul
;;                 |                 |  (max (* x1 y1) (* x2 y2)))
;; --------------------------------------------------------------------
;; -ve +ve -ve -ve | +ve +ve -ve -ve | (make-interval
;;                 |                 |  (* (min (abs y1) (abs y2)) x2)
;;                 |                 |  (* (max (abs y1) (abs y2)) x1))
;; --------------------------------------------------------------------
;; -ve -ve +ve +ve | -ve -ve -ve -ve | (make-interval
;;                 |                 |  (* (min x1 x2)
;;                 |                 |     (max y1 y2))
;;                 |                 |  (* (max x1 x2)
;;                 |                 |     (min y1 y2)))
;; --------------------------------------------------------------------
;; -ve -ve -ve +ve | +ve -ve +ve -ve | (make-interval
;;                 |                 |  (* (min x1 x2) y2)
;;                 |                 |  (* (min x1 x2) y1))
;; --------------------------------------------------------------------
;; -ve -ve -ve -ve | +ve +ve +ve +ve | (make-interval
;;                 |                 |  (* (min (abs x1) (abs x2))
;;                 |                 |     (min (abs y1) (abs y2)))
;;                 |                 |  (* (max (abs x1) (abs x2))
;;                 |                 |     (max (abs y1) (abs y2))))
;; ====================================================================


(define (all fun lst)
  (reduce (lambda (x y) (and x y)) #t (map fun lst)))


(define (mul-interval x y)
  (define (two-muls a b c d)    
    (make-interval (* a b) (* c d)))

  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))

    (cond ((all positive? (list x1 x2 y1 y2))
           (two-muls (min x1 x2)
                     (min y1 y2)
                     (max x1 x2)
                     (max y1 y2)))

          ((and (all positive? (list x1 x2 y2)) (negative? y1))
           (two-muls (max x1 x2)
                     y1
                     (max x1 x2)
                     y2))

          ((and (all positive? (list x1 x2)) (all negative? (list y1 y2)))
           (two-muls (max x1 x2)
                     (min y1 y2)
                     (min x1 x2)
                     (max y1 y2)))

          ((and (negative? x1) (all positive? (list x2 y1 y2)))
           (two-muls x1 (max y1 y2) x2 (max y1 y2)))

          ((and (positive? x2) (all negative? (list x1 y1 y2)))
           (two-muls (min (abs y1) (abs y2))
                     x2
                     (max (abs y1) (abs y2))
                     x1))

          ((and (all negative? (list x1 x2)) (all positive? (list y1 y2)))
           (two-muls (min x1 x2)
                     (max y1 y2)
                     (max x1 x2)
                     (min y1 y2)))

          ((and (all negative? (list x1 x2 y1)) (positive? y2))
           (two-muls (min x1 x2)
                     y2
                     (min x1 x2)
                     y1))

          ((all negative? (list x1 x2 y1 y2))
           (two-muls (min (abs x1) (abs x2))
                     (min (abs y1) (abs y2))
                     (max (abs x1) (abs x2))
                     (max (abs y1) (abs y2))))

          ;; Case of more than 2 multiplications
          ((and (all negative? (list x1 y1)) (all positive? (list x2 y2)))
           (make-interval (min (* x1 y2) (* x2 y1))
                          (max (* x1 y1) (* x2 y2))))

          (else (error "This case is not possible")))))


;; Tests
(equal? (make-interval 3 8) 
        (mul-interval (make-interval 1 2) (make-interval 3 4)))

(equal? (make-interval -4 8)
        (mul-interval (make-interval -1 2) (make-interval 3 4)))

(equal? (make-interval -8 -3)
        (mul-interval (make-interval -1 -2) (make-interval 3 4)))

(equal? (make-interval -8 -3)
        (mul-interval (make-interval 1 2) (make-interval -4 -3)))

(equal? (make-interval -4 8)
        (mul-interval (make-interval -1 2) (make-interval 3 4)))

(equal? (make-interval -6 8)
        (mul-interval (make-interval -1 2) (make-interval -3 4)))

(equal? (make-interval -6 8)
        (mul-interval (make-interval 1 2) (make-interval -3 4)))

(equal? (make-interval -8 6)
        (mul-interval (make-interval -2 -1) (make-interval -3 4)))

(equal? (make-interval 3 8)
        (mul-interval (make-interval -2 -1) (make-interval -4 -3)))

