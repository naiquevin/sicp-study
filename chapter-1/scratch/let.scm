

;; f(x, y) = x(1 + xy)^2 + y(1 - y) + (1 + xy)(1 - y)

(define (square x)
  (* x x))


(define (f x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (f-helper (+ 1 (* x y))
            (- 1 y)))


(define (f2 x y)
  ((lambda (a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
   (+ 1 (* x y))
   (- 1 y)))


(define (flet x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

(define x 5)

(+ (let ((x 3))
     (+ x (* x 10)))
   x)

(define x 2)

(let ((x 3)
      (y (+ x 2))) ; here x is 2 and not 3
  (* x y)) ; x is 3 and y is 4 here in the body only
    
  
