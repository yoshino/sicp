(define x (list 1 2 3))
(define y (list 4 5 6))

(gosh> append x y)
(1 2 3 4 5 6)
gosh> (cons x y)
((1 2 3) 4 5 6)
gosh> (list x y)
((1 2 3) (4 5 6))
