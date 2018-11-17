; それぞれのリストから7を取り出すには？

; l1
gosh> (cons 1 (cons 3 (cons (list 5 7) (list 9))))
(1 3 (5 7) 9)
gosh> (define l1 (cons 1 (cons 3 (cons (list 5 7) (list 9)))))
l1
gosh> l1
(1 3 (5 7) 9)
gosh> (cdr l1)
(3 (5 7) 9)
gosh> (cdr (cdr l1))
((5 7) 9)
gosh> (car (cdr (cdr l1)))
(5 7)
gosh> (cdr (car (cdr (cdr l1))))
(7)
gosh> (car (cdr (car (cdr (cdr l1)))))
7

; l2
gosh> (list (list 7))
((7))
gosh> (define l2 (list (list 7)))
l2
gosh> l2
((7))
gosh> (car l2)
(7)
gosh> (car (car l2))
7

; l3

gosh> (define l3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
l3
gosh> l3
(1 (2 (3 (4 (5 (6 7))))))
gosh> l3
(1 (2 (3 (4 (5 (6 7))))))
gosh> (cdr l3)
((2 (3 (4 (5 (6 7))))))
gosh> (car (cdr l3))
(2 (3 (4 (5 (6 7)))))
gosh> (cdr (car (cdr l3)))
((3 (4 (5 (6 7)))))
gosh> (car (cdr (car (cdr l3))))
(3 (4 (5 (6 7))))
gosh> (car (cdr (car (cdr (car (cdr (car (cdr l3))))))))
(5 (6 7))
gosh> (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3)))))))))))
(7)
gosh> (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr l3))))))))))))
7
