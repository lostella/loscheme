(define (count a b)
    (if (>= a b) b (count (+ a 1) b)))

(count 0 10000)
