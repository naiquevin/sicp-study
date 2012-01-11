;; Pascal's triangle

;;     1
;;    1 1
;;   1 2 1
;;  1 3 3 1
;; 1 4 6 4 1
;; .........

;; n

(define (pascals-elem row index)
  (if (or (= row 1) (= index 1) (= row index))
      1
      (+ (pascals-elem (- row 1) (- index 1))
         (pascals-elem (- row 1) index))))

(define (pascal row index)
  (cond ((< row index) #f)
        ((or (= row 1) (= index 1) (= row index)) 1)
        (else (+ (pascal (- row 1) (- index 1))
                 (pascal (- row 1) index)))))


              


