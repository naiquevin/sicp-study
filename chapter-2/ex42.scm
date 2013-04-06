;; Exercise 2.42
;;
;; The ``eight-queens puzzle`` asks how to place eight queens on a
;; chessboard so that no queen is in check from any other (i.e., no
;; two queens are in the same row, column, or diagonal). One possible
;; solution is shown in figure 2.8. One way to solve the puzzle is to
;; work across the board, placing a queen in each column. Once we have
;; placed k - 1 queens, we must place the kth queen in a position
;; where it does not check any of the queens already on the board. We
;; can formulate this approach recursively: Assume that we have
;; already generated the sequence of all possible ways to place k - 1
;; queens in the first k - 1 columns of the board. For each of these
;; ways, generate an extended set of positions by placing a queen in
;; each row of the kth column. Now filter these, keeping only the
;; positions for which the queen in the kth column is safe with
;; respect to the other queens. This produces the sequence of all ways
;; to place k queens in the first k columns. By continuing this
;; process, we will produce not only one solution, but all solutions
;; to the puzzle.  We implement this solution as a procedure queens,
;; which returns a sequence of all solutions to the problem of placing
;; n queens on an n× n chessboard. Queens has an internal procedure
;; queen-cols that returns the sequence of all ways to place queens in
;; the first k columns of the board.
;;
;; In this procedure rest-of-queens is a way to place k - 1 queens
;; in the first k - 1 columns, and new- row is a proposed row in which
;; to place the queen for the kth column. Complete the program by
;; implementing the representation for sets of board positions,
;; including the procedure adjoin-position, which adjoins a new
;; row-column position to a set of positions, and empty-board, which
;; represents an empty set of positions. You must also write the
;; procedure safe?, which determines for a set of positions, whether
;; the queen in the kth column is safe with respect to the
;; others. (Note that we need only check whether the new queen is safe
;; -- the other queens are already guaranteed safe with respect to
;; each other.


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


(define empty-board '())

;; abstracting away the representation of a position
(define (make-position col row)
  (cons col row))

(define (position-col position)
  (car position))

(define (position-row position)
  (cdr position))


(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens (list (make-position k new-row))))

;; Testing some intermediate state
;;
;; (flatmap
;;  (lambda (rest-of-queens)
;;    (map (lambda (new-row)
;;           (adjoin-position new-row 3 rest-of-queens))
;;         (enumerate-interval 1 8)))
;;  '(((1 . 1) (2 . 1)) ((1 . 1) (2 . 2)) ((1 . 1) (2 . 3)) ((1 . 1) (2 . 4)) ((1 . 1) (2 . 5)) ((1 . 1) (2 . 6)) ((1 . 1) (2 . 7)) ((1 . 1) (2 . 8)) ((1 . 2) (2 . 1)) ((1 . 2) (2 . 2)) ((1 . 2) (2 . 3)) ((1 . 2) (2 . 4)) ((1 . 2) (2 . 5)) ((1 . 2) (2 . 6)) ((1 . 2) (2 . 7)) ((1 . 2) (2 . 8)) ((1 . 3) (2 . 1)) ((1 . 3) (2 . 2)) ((1 . 3) (2 . 3)) ((1 . 3) (2 . 4)) ((1 . 3) (2 . 5)) ((1 . 3) (2 . 6)) ((1 . 3) (2 . 7)) ((1 . 3) (2 . 8)) ((1 . 4) (2 . 1)) ((1 . 4) (2 . 2)) ((1 . 4) (2 . 3)) ((1 . 4) (2 . 4)) ((1 . 4) (2 . 5)) ((1 . 4) (2 . 6)) ((1 . 4) (2 . 7)) ((1 . 4) (2 . 8)) ((1 . 5) (2 . 1)) ((1 . 5) (2 . 2)) ((1 . 5) (2 . 3)) ((1 . 5) (2 . 4)) ((1 . 5) (2 . 5)) ((1 . 5) (2 . 6)) ((1 . 5) (2 . 7)) ((1 . 5) (2 . 8)) ((1 . 6) (2 . 1)) ((1 . 6) (2 . 2)) ((1 . 6) (2 . 3)) ((1 . 6) (2 . 4)) ((1 . 6) (2 . 5)) ((1 . 6) (2 . 6)) ((1 . 6) (2 . 7)) ((1 . 6) (2 . 8)) ((1 . 7) (2 . 1)) ((1 . 7) (2 . 2)) ((1 . 7) (2 . 3)) ((1 . 7) (2 . 4)) ((1 . 7) (2 . 5)) ((1 . 7) (2 . 6)) ((1 . 7) (2 . 7)) ((1 . 7) (2 . 8)) ((1 . 8) (2 . 1)) ((1 . 8) (2 . 2)) ((1 . 8) (2 . 3)) ((1 . 8) (2 . 4)) ((1 . 8) (2 . 5)) ((1 . 8) (2 . 6)) ((1 . 8) (2 . 7)) ((1 . 8) (2 . 8))))
;;
;; (flatmap
;;  (lambda (rest-of-queens)
;;    (map (lambda (new-row)
;;           (adjoin-position new-row 1 rest-of-queens))
;;         (enumerate-interval 1 8)))
;;  (list '()))


(define (all items)
  (fold-left (lambda (x y) (and x y)) #t items))


(define (safe? k positions)

  (define (same-row? a b)
    (= (position-row a) (position-row b)))

  (define (same-diagonal? a b)
    (= (abs (- (position-col a) (position-col b)))
       (abs (- (position-row a) (position-row b)))))

  (let ((positions-rev (reverse positions)))
    (let ((kth (car positions-rev))
          (rest (cdr positions-rev)))
      (all
       (map (lambda (p)
              (and (not (same-row? p kth))
                   (not (same-diagonal? p kth))))
            rest)))))

;; (safe? 2 (list '(1 . 1) '(2 . 3)))

;; Note: Not used `k` argument in safe? Used the last element of the
;; list as kth position. To verify: Is this the sane thing to do?


;; Testing with smaller numbers first for sanity

(queens 1)
;Value 33: (((1 . 1)))

(queens 2)
;Value: ()

(queens 3)
;Value: ()

(queens 4)
;Value 34: (((1 . 2) (2 . 4) (3 . 1) (4 . 3))
;           ((1 . 3) (2 . 1) (3 . 4) (4 . 2)))

(define 8-queen-solutions (queens 8))

(length 8-queen-solutions)
;Value: 92

;; All 92 solutions

8-queen-solutions
;Value 36: (((1 . 1) (2 . 5) (3 . 8) (4 . 6) (5 . 3) (6 . 7) (7 . 2) (8 . 4))
;           ((1 . 1) (2 . 6) (3 . 8) (4 . 3) (5 . 7) (6 . 4) (7 . 2) (8 . 5))
;           ((1 . 1) (2 . 7) (3 . 4) (4 . 6) (5 . 8) (6 . 2) (7 . 5) (8 . 3))
;           ((1 . 1) (2 . 7) (3 . 5) (4 . 8) (5 . 2) (6 . 4) (7 . 6) (8 . 3))
;           ((1 . 2) (2 . 4) (3 . 6) (4 . 8) (5 . 3) (6 . 1) (7 . 7) (8 . 5))
;           ((1 . 2) (2 . 5) (3 . 7) (4 . 1) (5 . 3) (6 . 8) (7 . 6) (8 . 4))
;           ((1 . 2) (2 . 5) (3 . 7) (4 . 4) (5 . 1) (6 . 8) (7 . 6) (8 . 3))
;           ((1 . 2) (2 . 6) (3 . 1) (4 . 7) (5 . 4) (6 . 8) (7 . 3) (8 . 5))
;           ((1 . 2) (2 . 6) (3 . 8) (4 . 3) (5 . 1) (6 . 4) (7 . 7) (8 . 5))
;           ((1 . 2) (2 . 7) (3 . 3) (4 . 6) (5 . 8) (6 . 5) (7 . 1) (8 . 4))
;           ((1 . 2) (2 . 7) (3 . 5) (4 . 8) (5 . 1) (6 . 4) (7 . 6) (8 . 3))
;           ((1 . 2) (2 . 8) (3 . 6) (4 . 1) (5 . 3) (6 . 5) (7 . 7) (8 . 4))
;           ((1 . 3) (2 . 1) (3 . 7) (4 . 5) (5 . 8) (6 . 2) (7 . 4) (8 . 6))
;           ((1 . 3) (2 . 5) (3 . 2) (4 . 8) (5 . 1) (6 . 7) (7 . 4) (8 . 6))
;           ((1 . 3) (2 . 5) (3 . 2) (4 . 8) (5 . 6) (6 . 4) (7 . 7) (8 . 1))
;           ((1 . 3) (2 . 5) (3 . 7) (4 . 1) (5 . 4) (6 . 2) (7 . 8) (8 . 6))
;           ((1 . 3) (2 . 5) (3 . 8) (4 . 4) (5 . 1) (6 . 7) (7 . 2) (8 . 6))
;           ((1 . 3) (2 . 6) (3 . 2) (4 . 5) (5 . 8) (6 . 1) (7 . 7) (8 . 4))
;           ((1 . 3) (2 . 6) (3 . 2) (4 . 7) (5 . 1) (6 . 4) (7 . 8) (8 . 5))
;           ((1 . 3) (2 . 6) (3 . 2) (4 . 7) (5 . 5) (6 . 1) (7 . 8) (8 . 4))
;           ((1 . 3) (2 . 6) (3 . 4) (4 . 1) (5 . 8) (6 . 5) (7 . 7) (8 . 2))
;           ((1 . 3) (2 . 6) (3 . 4) (4 . 2) (5 . 8) (6 . 5) (7 . 7) (8 . 1))
;           ((1 . 3) (2 . 6) (3 . 8) (4 . 1) (5 . 4) (6 . 7) (7 . 5) (8 . 2))
;           ((1 . 3) (2 . 6) (3 . 8) (4 . 1) (5 . 5) (6 . 7) (7 . 2) (8 . 4))
;           ((1 . 3) (2 . 6) (3 . 8) (4 . 2) (5 . 4) (6 . 1) (7 . 7) (8 . 5))
;           ((1 . 3) (2 . 7) (3 . 2) (4 . 8) (5 . 5) (6 . 1) (7 . 4) (8 . 6))
;           ((1 . 3) (2 . 7) (3 . 2) (4 . 8) (5 . 6) (6 . 4) (7 . 1) (8 . 5))
;           ((1 . 3) (2 . 8) (3 . 4) (4 . 7) (5 . 1) (6 . 6) (7 . 2) (8 . 5))
;           ((1 . 4) (2 . 1) (3 . 5) (4 . 8) (5 . 2) (6 . 7) (7 . 3) (8 . 6))
;           ((1 . 4) (2 . 1) (3 . 5) (4 . 8) (5 . 6) (6 . 3) (7 . 7) (8 . 2))
;           ((1 . 4) (2 . 2) (3 . 5) (4 . 8) (5 . 6) (6 . 1) (7 . 3) (8 . 7))
;           ((1 . 4) (2 . 2) (3 . 7) (4 . 3) (5 . 6) (6 . 8) (7 . 1) (8 . 5))
;           ((1 . 4) (2 . 2) (3 . 7) (4 . 3) (5 . 6) (6 . 8) (7 . 5) (8 . 1))
;           ((1 . 4) (2 . 2) (3 . 7) (4 . 5) (5 . 1) (6 . 8) (7 . 6) (8 . 3))
;           ((1 . 4) (2 . 2) (3 . 8) (4 . 5) (5 . 7) (6 . 1) (7 . 3) (8 . 6))
;           ((1 . 4) (2 . 2) (3 . 8) (4 . 6) (5 . 1) (6 . 3) (7 . 5) (8 . 7))
;           ((1 . 4) (2 . 6) (3 . 1) (4 . 5) (5 . 2) (6 . 8) (7 . 3) (8 . 7))
;           ((1 . 4) (2 . 6) (3 . 8) (4 . 2) (5 . 7) (6 . 1) (7 . 3) (8 . 5))
;           ((1 . 4) (2 . 6) (3 . 8) (4 . 3) (5 . 1) (6 . 7) (7 . 5) (8 . 2))
;           ((1 . 4) (2 . 7) (3 . 1) (4 . 8) (5 . 5) (6 . 2) (7 . 6) (8 . 3))
;           ((1 . 4) (2 . 7) (3 . 3) (4 . 8) (5 . 2) (6 . 5) (7 . 1) (8 . 6))
;           ((1 . 4) (2 . 7) (3 . 5) (4 . 2) (5 . 6) (6 . 1) (7 . 3) (8 . 8))
;           ((1 . 4) (2 . 7) (3 . 5) (4 . 3) (5 . 1) (6 . 6) (7 . 8) (8 . 2))
;           ((1 . 4) (2 . 8) (3 . 1) (4 . 3) (5 . 6) (6 . 2) (7 . 7) (8 . 5))
;           ((1 . 4) (2 . 8) (3 . 1) (4 . 5) (5 . 7) (6 . 2) (7 . 6) (8 . 3))
;           ((1 . 4) (2 . 8) (3 . 5) (4 . 3) (5 . 1) (6 . 7) (7 . 2) (8 . 6))
;           ((1 . 5) (2 . 1) (3 . 4) (4 . 6) (5 . 8) (6 . 2) (7 . 7) (8 . 3))
;           ((1 . 5) (2 . 1) (3 . 8) (4 . 4) (5 . 2) (6 . 7) (7 . 3) (8 . 6))
;           ((1 . 5) (2 . 1) (3 . 8) (4 . 6) (5 . 3) (6 . 7) (7 . 2) (8 . 4))
;           ((1 . 5) (2 . 2) (3 . 4) (4 . 6) (5 . 8) (6 . 3) (7 . 1) (8 . 7))
;           ((1 . 5) (2 . 2) (3 . 4) (4 . 7) (5 . 3) (6 . 8) (7 . 6) (8 . 1))
;           ((1 . 5) (2 . 2) (3 . 6) (4 . 1) (5 . 7) (6 . 4) (7 . 8) (8 . 3))
;           ((1 . 5) (2 . 2) (3 . 8) (4 . 1) (5 . 4) (6 . 7) (7 . 3) (8 . 6))
;           ((1 . 5) (2 . 3) (3 . 1) (4 . 6) (5 . 8) (6 . 2) (7 . 4) (8 . 7))
;           ((1 . 5) (2 . 3) (3 . 1) (4 . 7) (5 . 2) (6 . 8) (7 . 6) (8 . 4))
;           ((1 . 5) (2 . 3) (3 . 8) (4 . 4) (5 . 7) (6 . 1) (7 . 6) (8 . 2))
;           ((1 . 5) (2 . 7) (3 . 1) (4 . 3) (5 . 8) (6 . 6) (7 . 4) (8 . 2))
;           ((1 . 5) (2 . 7) (3 . 1) (4 . 4) (5 . 2) (6 . 8) (7 . 6) (8 . 3))
;           ((1 . 5) (2 . 7) (3 . 2) (4 . 4) (5 . 8) (6 . 1) (7 . 3) (8 . 6))
;           ((1 . 5) (2 . 7) (3 . 2) (4 . 6) (5 . 3) (6 . 1) (7 . 4) (8 . 8))
;           ((1 . 5) (2 . 7) (3 . 2) (4 . 6) (5 . 3) (6 . 1) (7 . 8) (8 . 4))
;           ((1 . 5) (2 . 7) (3 . 4) (4 . 1) (5 . 3) (6 . 8) (7 . 6) (8 . 2))
;           ((1 . 5) (2 . 8) (3 . 4) (4 . 1) (5 . 3) (6 . 6) (7 . 2) (8 . 7))
;           ((1 . 5) (2 . 8) (3 . 4) (4 . 1) (5 . 7) (6 . 2) (7 . 6) (8 . 3))
;           ((1 . 6) (2 . 1) (3 . 5) (4 . 2) (5 . 8) (6 . 3) (7 . 7) (8 . 4))
;           ((1 . 6) (2 . 2) (3 . 7) (4 . 1) (5 . 3) (6 . 5) (7 . 8) (8 . 4))
;           ((1 . 6) (2 . 2) (3 . 7) (4 . 1) (5 . 4) (6 . 8) (7 . 5) (8 . 3))
;           ((1 . 6) (2 . 3) (3 . 1) (4 . 7) (5 . 5) (6 . 8) (7 . 2) (8 . 4))
;           ((1 . 6) (2 . 3) (3 . 1) (4 . 8) (5 . 4) (6 . 2) (7 . 7) (8 . 5))
;           ((1 . 6) (2 . 3) (3 . 1) (4 . 8) (5 . 5) (6 . 2) (7 . 4) (8 . 7))
;           ((1 . 6) (2 . 3) (3 . 5) (4 . 7) (5 . 1) (6 . 4) (7 . 2) (8 . 8))
;           ((1 . 6) (2 . 3) (3 . 5) (4 . 8) (5 . 1) (6 . 4) (7 . 2) (8 . 7))
;           ((1 . 6) (2 . 3) (3 . 7) (4 . 2) (5 . 4) (6 . 8) (7 . 1) (8 . 5))
;           ((1 . 6) (2 . 3) (3 . 7) (4 . 2) (5 . 8) (6 . 5) (7 . 1) (8 . 4))
;           ((1 . 6) (2 . 3) (3 . 7) (4 . 4) (5 . 1) (6 . 8) (7 . 2) (8 . 5))
;           ((1 . 6) (2 . 4) (3 . 1) (4 . 5) (5 . 8) (6 . 2) (7 . 7) (8 . 3))
;           ((1 . 6) (2 . 4) (3 . 2) (4 . 8) (5 . 5) (6 . 7) (7 . 1) (8 . 3))
;           ((1 . 6) (2 . 4) (3 . 7) (4 . 1) (5 . 3) (6 . 5) (7 . 2) (8 . 8))
;           ((1 . 6) (2 . 4) (3 . 7) (4 . 1) (5 . 8) (6 . 2) (7 . 5) (8 . 3))
;           ((1 . 6) (2 . 8) (3 . 2) (4 . 4) (5 . 1) (6 . 7) (7 . 5) (8 . 3))
;           ((1 . 7) (2 . 1) (3 . 3) (4 . 8) (5 . 6) (6 . 4) (7 . 2) (8 . 5))
;           ((1 . 7) (2 . 2) (3 . 4) (4 . 1) (5 . 8) (6 . 5) (7 . 3) (8 . 6))
;           ((1 . 7) (2 . 2) (3 . 6) (4 . 3) (5 . 1) (6 . 4) (7 . 8) (8 . 5))
;           ((1 . 7) (2 . 3) (3 . 1) (4 . 6) (5 . 8) (6 . 5) (7 . 2) (8 . 4))
;           ((1 . 7) (2 . 3) (3 . 8) (4 . 2) (5 . 5) (6 . 1) (7 . 6) (8 . 4))
;           ((1 . 7) (2 . 4) (3 . 2) (4 . 5) (5 . 8) (6 . 1) (7 . 3) (8 . 6))
;           ((1 . 7) (2 . 4) (3 . 2) (4 . 8) (5 . 6) (6 . 1) (7 . 3) (8 . 5))
;           ((1 . 7) (2 . 5) (3 . 3) (4 . 1) (5 . 6) (6 . 8) (7 . 2) (8 . 4))
;           ((1 . 8) (2 . 2) (3 . 4) (4 . 1) (5 . 7) (6 . 5) (7 . 3) (8 . 6))
;           ((1 . 8) (2 . 2) (3 . 5) (4 . 3) (5 . 1) (6 . 7) (7 . 4) (8 . 6))
;           ((1 . 8) (2 . 3) (3 . 1) (4 . 6) (5 . 2) (6 . 5) (7 . 7) (8 . 4))
;           ((1 . 8) (2 . 4) (3 . 1) (4 . 3) (5 . 6) (6 . 2) (7 . 7) (8 . 5)))
