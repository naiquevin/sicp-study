;; Exercise 2.43. Louis Reasoner is having a terrible time doing
;; exercise 2.42. His queens procedure seems to work, but it runs
;; extremely slowly. (Louis never does manage to wait long enough for
;; it to solve even the 6× 6 case.) When Louis asks Eva Lu Ator for
;; help, she points out that he has interchanged the order of the
;; nested mappings in the flatmap, writing it as

;; (flatmap
;;  (lambda (new-row)
;;    (map (lambda (rest-of-queens)
;;           (adjoin-position new-row k rest-of-queens))
;;         (queen-cols (- k 1))))
;;  (enumerate-interval 1 board-size))

;; Explain why this interchange makes the program run slowly. Estimate
;; how long it will take Louis's program to solve the eight-queens
;; puzzle, assuming that the program in exercise 2.42 solves the
;; puzzle in time T.

In the correct and efficient version of the solution, in one
invocation of (queen-cols k), (queen-cols (- k 1)) is called only
once. Interchanging the order of nested mappings results in the
function (queen-cols (- k 1)) being called `x` times (where x is the
length of previous solutions), although the value of k doesn't change
in 1 iteration. This is the reason why Louis Reasoner's solution runs
very slowly even for small values of `board-size` as there is a lot of
redundant work going on in each interation.

A recursive function being called multiple times inside one iteration
results in tree recursion which grows exponentially. So if the time
taken by efficient solution is `T`, then this solution will take
around `T^board-size`.
