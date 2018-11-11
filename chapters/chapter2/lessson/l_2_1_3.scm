(define (cons x y)
  (define (dispatch m)
    (cond
      ((= m 0) x)
      ((= m 1) y)
      (else (error "Arguments not 0 or 1: CONS:" m))))
  dispatch)
(define (car z) (z 0))
(define (cdr z) (z 1))

; gosh> (cons 3 4)
; #<closure ((cons dispatch) m)>
; gosh> ((cons 3 4) 1)
; 4
