;a
(define (for-each proc items)
  (if (null? items)
      'done
      (begin (proc (car items))
             (for-each proc (cdr items)))))

;(for-each (lambda (x) (newline) (display x)) ; 基本式：遅延が行われない
;          '(57 321 88))

; 元々の実装
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))
; 新しい実装
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env) ; changed!!
              (eval-sequence (rest-exps exps) env))))

; 返り値は変更しない
; として正確に評価されるので
;(begin ((lambda (x) (newline) (display x) 57)
;       (for-each proc (cdr items)))))

;b
(define (p1 x)
  (set! x (cons x '(2)))
  x)

(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))

;元々の実装
;(p1 1)
;#=>(1 2) ; x: (1, 2)
;(p2 1)
;#=>1

;;; M-Eval input:
;(p1 1)
;#=>(1 2)
;(p2 1)
;#=>(1 2)

;c
; aでの答えに等しい
; aの処理は基本手続きなので影響をうけない
