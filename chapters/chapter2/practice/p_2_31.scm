(define (tree-map factor tree)
  (cond
    ((null? tree) ())
    ((not (pair? tree)) (factor tree))
    (else (cons (tree-map factor (car tree))
                (tree-map factor (cdr tree))))))

(define (square-tree tree) (tree-map square tree))

(define t (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;gosh> t
;(1 (2 (3 4) 5) (6 7))
;gosh> (tree-map square t)
;(1 (4 (9 16) 25) (36 49))
