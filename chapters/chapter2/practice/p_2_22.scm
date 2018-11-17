(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons (square (car things)) answer))))
  (iter items ()))

; gosh> (square-list (list 1 2 3))
; (9 4 1)

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer (square (car things))))))
  (iter items ()))

; gosh> (square-list (list 1 2 3))
; (((() . 1) . 4) . 9)

; cons引数に対して非対称性にふるまう
; 第2引数がリストであれば、そのリストの中に第一引数をいれる
; 第2引数がリストでなければ、別のリストの中に第一引数と第二引数を入れる形になる
; gosh> (cons 1 2)
; (1 . 2)
; gosh> (cons 1 ())
; (1)
; gosh> (cons () 1)
; (() . 1)
