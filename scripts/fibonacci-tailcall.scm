(define (fib n)
  (define (fib-tail-rec n a b)
    (if (= n 0)
        a
        (fib-tail-rec (- n 1) b (+ a b))))
  (fib-tail-rec n 0 1))

(fib 50)
