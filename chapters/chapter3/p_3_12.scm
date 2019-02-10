(define (append x y)
  (if (null? x)
    y
    (cons (car x) (append (cdr x) y))))

;gosh> (append (list 1 2) (list 3 4))
;(1 2 3 4)
;gosh> (cons (list 1 2) (list 3 4))
;((1 2) 3 4)

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair? x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

; ここまでが与えられている

(define x (list 'a 'b))
(define y (list 'c 'd))

(define z1 (append x y))
;gosh> z1
;(a b c d)
;gosh> (cdr x)
;(b)

; x -> a,b はそのまま

(define z2 (append! x y))
;gosh> z2
;(a b c d)
;gosh> (cdr x)
;(b c d)
;gosh> (cdr z2)
;(b c d)

; z2 -> x -> 1, 2, 3, 4
