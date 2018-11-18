(define (scale-tree tree factor)
  (cond
    ((null? tree) ())
    ((not (pair? tree)) (* tree factor))
    (else (cons (scale-tree (car tree) factor)
                (scale-tree (cdr tree) factor)))))


; square-tree
(define (square-tree tree)
  (cond
    ((null? tree) ())
    ((not (pair? tree)) (square tree))
    (else (cons (square-tree (car tree))
                (square-tree (cdr tree))))))

(define t (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;gosh> t
;(1 (2 (3 4) 5) (6 7))
;gosh> (square-tree t)
;(1 (4 (9 16) 25) (36 49))

; square-tree
; 反復
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (square-tree sub-tree)
           (square sub-tree)))
       tree))
