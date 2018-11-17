(define (same-parity . a)
  (if (= (remainder (car a) 2) 0)
    (even-list a (list))
    (odd-list a (list))))

(define (even-list a result)
  (cond
    ((null? a) result)
    ((= (remainder (car a) 2) 0) (even-list (cdr a) (cons (car a) result)))
    (else (even-list (cdr a) result))))

(define (odd-list a result)
  (cond
    ((null? a) result)
    ((= (remainder (car a) 3) 0) (even-list (cdr a) (cons (car a) result)))
    (else (even-list (cdr a) result))))
