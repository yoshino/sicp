;------------------------------------------------------
; cond
;------------------------------------------------------
; ((assoc 'b '((a 1) (b 2))) => cadr))
; (assoc 'b '((a 1) (b 2))の評価値がcadrの引数として与えられ評価されるようにしたい

; ((=>? exp) (eval-cond-part exp)) みたいにcondのなかで定義する
(define (eval-cond-part exp)
  (let
    ((evaluated (eval (car exp) (interaction-environment))))
    (if evaluated
        (apply (eval (caddr e) (interaction-environment)) (list evaluated))
        #f)))

(define e '((assoc 'b '((a 1) (b 2))) => cadr))
(define res-c (eval-cond-part e))
;gosh> res-c
;2

; 試行錯誤
;gosh> (apply (eval (caddr e) (interaction-environment)) '(b 2))
;*** ERROR: wrong number of arguments for #<subr (cadr obj)> (required 1, got 2)
;gosh> (apply (eval (caddr e) (interaction-environment)) (list '(b 2)))
;2
