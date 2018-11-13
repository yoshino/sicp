(define (reverse items)
  (reverse-iter items (list)))

(define (last-pair items)
  (list-ref items (- (length items) 1)))

(define (reverse-iter items result)
  (if (null? items)
    result
    (reverse-iter (cdr items) (cons (car items) result))))
