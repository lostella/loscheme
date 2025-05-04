(define (cond-recursion-1 n)
  (define (loop count)
    (cond
      ((> count 0) (loop (- count 1)))
      (else 'done)))
  (loop n))

(write (cond-recursion-1 1000000))
(newline)

(define (cond-recursion-2 n)
  (define (loop count)
    (cond
      ((zero? count) 'done)
      (else (loop (- count 1)))))
  (loop n))

(write (cond-recursion-2 1000000))
(newline)
