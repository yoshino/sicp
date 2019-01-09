;------------------------------------------------------
; let
;------------------------------------------------------

;こんな感じの動作ができればOK!
;gosh> e
;(let ((one 1) (two 2)) (+ one two))
;gosh> (eval e (interaction-environment))
;3

; これはlambdaで以下のように置き換えられる
;gosh> ((lambda (one two) (+ one two)) 1 2)
;3

; 定義部分
;gosh> (car (cdr e))
;((one 1) (two 2))
; 適用部分
;gosh> (car (cdr (cdr e)))
;(+ one two)

(define (cons-car pairs)
  (let ((result '()))
    (define (loop pairs)
      (if (null? pairs)
        result
        (begin
          (set! result (append result (list (car (car pairs)))))
          (loop (cdr pairs)))))
    (loop pairs)))

(define l '((1 2) (3 4) (5 6)))
;gosh> (cons-car l)
;(1 3 5)

(define (cons-cdr pairs)
  (let ((result '()))
    (define (loop pairs)
      (if (null? pairs)
        result
        (begin
          (set! result (append result (cdr (car pairs))))
          (loop (cdr pairs)))))
    (loop pairs)))

(define l '((1 2) (3 4) (5 6)))
;gosh> (cons-cdr l)
;(2 4 6)

; ((let? exp) (let->combination exp)) みたいに定義する
; letをlambdaの派状式として定義する
;gosh> ((lambda (one two) (+ one two)) 1 2)
;gosh> ((eval (append (cons 'lambda (list '(one two))) (list'(+ 1 2))) (interaction-environment)) 1 2)
(define (let->combination exp)
  (define hash (apply list (car (cdr exp))))
  (define proc (apply list (car (cdr (cdr exp)))))
  (define vals (cons-cdr hash))
  (define keys (cons-car hash))
  (apply (eval (append (cons 'lambda (list keys)) (list proc)) (interaction-environment)) vals))

(define e '(let ((one 1) (two 2)) (+ one two)))
(define res-let (let->combination e))
;gosh> res-let
;3
