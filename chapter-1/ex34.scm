
;; Exercise 1.34. Suppose we define the procedure
;; (define (f g)
;; (g 2))

;; Then we have
;; (f square)
;; 4
;; (f (lambda (z) (* z (+ z 1))))
;; 6
;; What happens if we (perversely) ask the interpreter to evaluate the combination (f f)? Explain.

(define (f g)
  (g 2))

(f f)

;; It will produce an error

;; Explanation:
;; 
;; The function f is defined to accept a function as it's argument
;; When we call f with f itself, it returns (f 2) Here the argument is
;; a number and the function call will try to evaluate (2 2) which
;; will throw an error "The object 2 is not applicable"

