(define limit 100000)

(define (count-if-1 n)
  (if (not (zero? n))
      (count-if-1 (- n 1))
      n))

(write (count-if-1 limit))
(newline)

(define (count-if-2 n)
  (if (zero? n)
      n
      (count-if-2 (- n 1))))

(write (count-if-2 limit))
(newline)

(define (count-cond-1 n)
  (cond
    ((not (zero? n)) (count-cond-1 (- n 1)))
    (else n)))

(write (count-cond-1 limit))
(newline)

(define (count-cond-2 n)
  (cond
    ((zero? n) n)
    (else (count-cond-2 (- n 1)))))

(write (count-cond-2 limit))
(newline)

(define (no-op) (begin))

(define (count-begin n)
  (begin
    (no-op)
    (if (zero? n)
        n
        (count-begin (- n 1)))))

(write (count-begin limit))
(newline)

(define (count-and n)
  (cond
      ((zero? n) n)
      (else (and (not (zero? n))
            (count-and (- n 1))))))

(write (count-and limit))
(newline)

(define (count-or n)
  (cond
      ((zero? n) n)
      (else (or (zero? n)
            (count-or (- n 1))))))

(write (count-or limit))
(newline)

(define (count-mutual-rec n)
  (define (count-even n)
      (if (positive? n) (count-odd (- n 1)) n))
  (define (count-odd n)
      (if (positive? n) (count-even (- n 1)) n))
  (count-even n))

(write (count-mutual-rec limit))
(newline)
