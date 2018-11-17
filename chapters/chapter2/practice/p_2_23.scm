; mapは以下のように定義される
(define (scale-list items factor)
  (if (null? items)
    (cons (* (car items) factor)
          (scale-list (cdr items) factor))))

(define (for-each factor items)
  (cond
    ((null? items) #t)
    (else (factor (car items))
          (for-each factor (cdr items)))))
