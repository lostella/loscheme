(define (ackermann m n)
  (cond
    ((= m 0) (+ n 1))
    ((= n 0) (ackermann (- m 1) 1))
    (else (ackermann (- m 1) (ackermann m (- n 1))))))

(ackermann 3 6)
