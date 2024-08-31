(define (quicksort lst)
  (if (null? lst)
      '()
      (let ((pivot (car lst))
            (rest (cdr lst)))
        (append
          (quicksort (filter (lambda (x) (< x pivot)) rest))
          (list pivot)
          (quicksort (filter (lambda (x) (>= x pivot)) rest))))))

(quicksort '(34 7 23 32 5 62 32 2 1 6 45 78 99 3))
