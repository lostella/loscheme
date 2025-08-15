(import (scheme write))

(define (quicksort lst)
  (if (null? lst)
      '()
      (let ((pivot (car lst))
            (rest (cdr lst)))
        (append
          (quicksort (filter (lambda (x) (< x pivot)) rest))
          (list pivot)
          (quicksort (filter (lambda (x) (>= x pivot)) rest))))))

(define (build-random-list n)
  (define (rand n) (remainder (+ (* n 22695477) 1) 4294967296))
  (define (iter i seed acc)
    (if (= i 0)
        acc
        (let ((new-seed (rand seed)))
          (iter (- i 1) new-seed (cons (remainder new-seed 100000) acc)))))
  (iter n 42 '()))

(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

(define test-list (build-random-list 1000))
(define sorted (quicksort test-list))
(write (length sorted))
(newline)
(write (car sorted))
(newline)
(write (last sorted))
(newline)
