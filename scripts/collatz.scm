(define (collatz n)
  (define (collatz-helper n count)
    (cond
      ((= n 1) count)
      ((even? n) (collatz-helper (quotient n 2) (+ count 1)))
      (else (collatz-helper (+ (* 3 n) 1) (+ count 1)))))
  (collatz-helper n 1))

(collatz 837799)
