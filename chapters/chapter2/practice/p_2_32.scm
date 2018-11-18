(define (tree-map factor tree)
  (cond
    ((null? tree) ())
    ((not (pair? tree)) (factor tree))
    (else (cons (tree-map factor (car tree))
                (tree-map factor (cdr tree))))))

(define t (list 1 2 3))

(define (subsets s)
   (if (null? s)
       (list ())
       (let ((rest (subsets (cdr s))))
          (append rest (map (lambda (x) (cons (car s) x))
                            rest)))))

;(let ((rest (subsets (2 3))))
;  (append (2 3) (map
;                 (lambda (x) (cons 1 x))
;                 (2 3))))

; gosh> (map (lambda (x) (cons (car (list 1 2 3)) x)) (cdr (list 1 2 3)))
; ((1 . 2) (1 . 3))


;(let ((rest (subsets (2 3))))
;  (append (2 3) ((1 2) (1 3)))
