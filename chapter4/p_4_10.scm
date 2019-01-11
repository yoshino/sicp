;---------------------------------------------------------------
; evalとapplyを変更せずにletの挙動を変更する
;---------------------------------------------------------------
; 具体的には(let (key value) となっているものを
; (let value key にしてみる
(define (cons-car pairs)
  (let ((result '()))
    (define (loop pairs)
      (if (null? pairs)
        result
        (begin
          (set! result (append result (list (car (car pairs)))))
          (loop (cdr pairs)))))
    (loop pairs)))

(define (cons-cdr pairs)
  (let ((result '()))
    (define (loop pairs)
      (if (null? pairs)
        result
        (begin
          (set! result (append result (cdr (car pairs))))
          (loop (cdr pairs)))))
    (loop pairs)))

(define (let->combination exp)
  (define hash (apply list (car (cdr exp))))
  (define proc (apply list (car (cdr (cdr exp)))))
  (define keys (cons-cdr hash))
  (define vals (cons-car hash))
  (apply (eval (append (cons 'lambda (list keys)) (list proc)) (interaction-environment)) vals))

(define e '(let ((1 one) (2 two)) (+ one two)))
(define res-let (let->combination e))
; => 3

