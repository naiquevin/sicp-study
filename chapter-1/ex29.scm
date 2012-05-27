;; Simpson's Rule

(define (cube x)
  (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define term
    (lambda (k)
      (cond ((= k 0) (f (+ a (* k h))))
            ((even? k) (* 2 (f (+ a (* k h)))))
            (else (* 4 (f (+ a (* k h))))))))
  (define next
    (lambda (k)
      (+ 1 k)))
  (* (/ h 3.0) (sum term 0 next n)))


(simpson-integral cube 0 1 100) ; .25333333333333335

(simpson-integral cube 0 1 1000) ; .25033333333333335

