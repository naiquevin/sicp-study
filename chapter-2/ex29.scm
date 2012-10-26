;; Exercise 2.29. A binary mobile consists of two branches, a left
;; branch and a right branch. Each branch is a rod of a certain
;; length, from which hangs either a weight or another binary
;; mobile. We can represent a binary mobile using compound data by
;; constructing it from two branches (for example, using list):

(define (make-mobile left right)
  (list left right))

;; A branch is constructed from a length (which must be a number)
;; together with a structure, which may be either a number
;; (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

;; a. Write the corresponding selectors left-branch and right-branch,
;; which return the branches of a mobile, and branch-length and
;; branch-structure, which return the components of a branch.

;; b. Using your selectors, define a procedure total-weight that
;; returns the total weight of a mobile.

;; c. A mobile is said to be balanced if the torque applied by its
;; top-left branch is equal to that applied by its top-right branch
;; (that is, if the length of the left rod multiplied by the weight
;; hanging from that rod is equal to the corresponding product for the
;; right side) and if each of the submobiles hanging off its branches
;; is balanced. Design a predicate that tests whether a binary mobile
;; is balanced.

;; d. Suppose we change the representation of mobiles so that the
;; constructors are

;; (define (make-mobile left right)
;;   (cons left right))
;; (define (make-branch length structure)
;;   (cons length structure))

;; How much do you need to change your programs to convert to the new
;; representation?


;; First some branches and mobiles for testing

(define b1 (make-branch 10 100))
(define b2 (make-branch 15 80))

(define m1 (make-mobile b1 b2))

(define b3 (make-branch 20 140))
(define b4 (make-branch 20 m1))

(define m2 (make-mobile b3 b4))


;; Visualization of the tree

;;             [m2]
;;             / \
;;            /   \
;;           b3    b4
;;     l=20 /       \ l=20
;;         /         \
;;       {140}       m1
;;                  / \
;;                 /   \
;;                b1    b2
;;          l=10 /       \ l=15
;;              /         \
;;            {100}      {80}


;; a) left-branch, right-branch, branch-length, branch-structure
;; selectors

(define (first-of-two x)
  (car x))

(define (second-of-two x)
  (car (cdr x)))

(define (left-branch mobile)
  (first-of-two mobile))

(define (right-branch mobile)
  (second-of-two mobile))

(equal? b1 (left-branch m1))
(equal? b2 (right-branch m1))


(define (branch-length branch)
  (first-of-two branch))

(define (branch-structure branch)
  (second-of-two branch))

(= 20 (branch-length b3))
(equal? 140 (branch-structure b3))
(equal? m1 (branch-structure b4))


;; b) Total weight of the mobile using the above selectors

(define (mobile? structure)
  (pair? structure))

(define (total-weight structure)
  (if (not (mobile? structure))
      structure
      (+ (total-weight
          (branch-structure
           (left-branch structure)))
         (total-weight
          (branch-structure
           (right-branch structure))))))

(= 180 (total-weight m1))
(= 320 (total-weight m2))


;; c) Test for balanced mobile

(define (torque branch)
  (define (branch-weight branch)
    (let ((struct (branch-structure branch)))
      (if (not (mobile? struct))
          struct
          (total-weight struct))))
  (* (branch-length branch)
     (branch-weight branch)))


(define (balanced? structure)
  (if (not (mobile? structure))
      #t
      (let ((struct-left (branch-structure (left-branch structure)))
            (struct-right (branch-structure (right-branch structure)))
            (torque-left (torque (left-branch structure)))
            (torque-right (torque (right-branch structure))))
        (cond ((and
                (not (mobile? struct-left))
                (not (mobile? struct-right)))
               (= torque-left torque-right))
              ((and
                (mobile? struct-left)
                (not (mobile? struct-right)))
               (and (= torque-left torque-right)
                    (balanced? struct-left)))
              ((and
                (not (mobile? struct-left))
                (mobile? struct-right))
               (and (= torque-left torque-right)
                    (balanced? struct-right)))
              (else
               (and (= torque-left torque-right)
                    (balanced? struct-left)
                    (balanced? struct-right)))))))


(define m3 (make-mobile
            (make-branch 10 10)
            (make-branch 5 20)))


(balanced? m3)
;Value: #t

(define m4 (make-mobile
            (make-branch 10 100)
            (make-branch 5
                         (make-mobile
                          (make-branch 2 120)
                          (make-branch 3 80)))))

(balanced? m4)
;Value: #t

(balanced? m1)
;Value: #f

(balanced? m2)
;Value: #f

;; d. Changes after modifying the implementation of make-mobile and
;; make-branch

(define (second-of-two x)
  (cdr x))
