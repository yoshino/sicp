;gosh> (eq? 'a 'b)
;#f
;gosh> (eq? 'a 'a)
;#t
;gosh> (eq? (list 'a 'b) (list 'a 'c))
;#f
;gosh> (eq? (list 'a 'b) (list 'a 'b))
;#f
;gosh> (eq? '(a b) '(a b))
;#f
;gosh> (eq? 1 2)
;#f
;gosh> (eq? 1 1)
;#t

(define (equal? p1 p2)
  (cond ((and (null? p1) (null? p2)) #t)
        ((or (null? p1) (null? p2)) #f)
        ((and (pair? p1) (pair? p2))
         (and (equal? (car p1) (car p2))
              (equal? (cdr p1) (cdr p2))))
        ((or (pair? p1) (pair? p2)) #f)
        (else (eq? p1 p2))))
