(define (fold-right op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
    result
    (iter (op result (car rest)) (cdr rest))))
  (iter initial sequence))

; opが任意の列に対して、fold-right、fold-leftが同じ値を返すための条件は?
; 左右対称であること
;gosh> (fold-left + 0 (list 1 2 3))
;6
;gosh> (fold-right + 0 (list 1 2 3))
;6
;gosh> (fold-left cons () (list 1 2 3))
;(((() . 1) . 2) . 3)
;gosh> (fold-right cons () (list 1 2 3))
;(1 2 3)
