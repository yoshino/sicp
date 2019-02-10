(define (add a b) (+ a b))
(define (calc func a b) (func a b))

;手続きを引数に渡すことができる
;funcが引数として渡された時funcがまだ評価されていない
;(func a b) なのでこの式をapplyする時はfuncを評価済みにする必要がある

;gosh> (driver-loop)
;;; M-Eval input:
;(define (add a b) (+ a b))
;;; M-Eval value:
;ok
;;; M-Eval input:
;(define (calc func a b) (func a b))
;;; M-Eval value:
;ok
;;; M-Eval input:
;(calc add 1 2)
;;; M-Eval value:
;3

; actual-value ではなくevalを使う

;(apply (eval (operator exp) env)
;      (operands exp)
;      env))

;driver-loopの結果
;;; M-Eval input:
;(calc add 1 2)
;*** ERROR: Unknown procedure type -- APPLY (thunk add #0=(



