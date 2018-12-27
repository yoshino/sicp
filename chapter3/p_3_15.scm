(define x (list 'a 'b))
(define z1 (cons x x))

(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x) (set-car! (car x) 'wow) x)

;gosh> z1
;((a b) a b)
;gosh> (set-to-wow! z1)
;((wow b) wow b)
;gosh> z2
;((a b) a b)
;gosh> (set-to-wow! z2)
;((wow b) a b)


;((wow b) wow b)
; z1 => wow->b

;((wow b) a b)

; z2 -> wow->b
;    -> a  ->b
