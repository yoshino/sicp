(define (cons x y)
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          (else (error "Undefined operation: CONS" m))))
  dispatch)
(define (car z) (z 'car ))
(define (cdr z) (z 'cdr ))

;gosh> (define l (cons 2 4))
;gosh> l
;<closure ((cons dispatch) m)>
;gosh> (l 'car)
;2
;gosh> (l 'cdr)
;4
;gosh> (car l)
;2
;gosh> (cdr l)
;4

;可変データへの拡張
(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else
            (error "Undefined operation: CONS" m))))
  dispatch)
(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
  ((z 'set-car!) new-value) z)
(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value) z)

; これらの式を使って以下の式を評価する

(define x (cons 1 2))
(define z (cons x x))
(set-car! (cdr z) 17)
(car x)

;gosh> x
;#<closure ((cons dispatch) m)>
;gosh> z
;#<closure ((cons dispatch) m)>
;gosh> (set-car! (cdr z) 17)
;#<closure ((cons dispatch) m)>
;gosh> (car x)
;17
