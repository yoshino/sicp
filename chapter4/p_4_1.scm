;------------------------------------------------------------
; 4.1: 評価順序に関して
;------------------------------------------------------------
; 左から右へ評価するか？ 右から左へ評価するか？
; 実装する側の言語(scheme)に依存している
; １つずつ分解してevalしていっていて、この方向が左から右になっている。
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

;------------------------------------------------------------
; lispの評価順序に依存せず左から右へと評価するバージョン
;------------------------------------------------------------
(define (first-operand exps) (car exps))

(define val 10)
(define expression '((set! val (+ val 2)) (set! val (* val 2))))

(define (list-of-values-left-to-right exps)
  (if (null? exps)
      '()
      (let ((first-eval (eval (car exps) (interaction-environment))))
           (cons first-eval
                 (list-of-values-left-to-right (cdr exps))))))

; interaction-environmentは環境識別子で全てのGaucheのビルトインと
; ユーザ定義の全てを含んだuserモジュールを返します
;gosh> (eval (car expression) (interaction-environment))
;12
;gosh> (eval (car expression) (interaction-environment))手続き適用がcallで始まるときを考える4
;gosh> (eval (car expression) (interaction-environment))
;16

(define from-left (list-of-values-left-to-right expression))
;gosh> from-left
;(12 24)

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first-eval (eval (car exps) (interaction-environment))))
        (cons first-eval
            (list-of-values (rest-operands exps) env)))))

;------------------------------------------------------------
; 右から左へと評価するバージョン
;------------------------------------------------------------
(define (last ls)
  (if (null? (cdr ls))
      (car ls)
      (last (cdr ls))))

(define (remove-last ls)
  (let ((resut '()))
    (if (null? (cdr ls))
      resut
      (cons (car ls) (remove-last (cdr ls))))))

(define val 10)
(define expression '((set! val (+ val 2)) (set! val (* val 2))))

(define (list-of-values-right-to-left exps)
  (if (null? exps)
      '()
      (let ((first-eval (eval (last exps) (interaction-environment)))
            (last-removed-exps (remove-last exps)))
        (cons first-eval (list-of-values-right-to-left last-removed-exps)))))

(define from-right (list-of-values-right-to-left expression))
;gosh> from-right
;(20 22)

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (let ((first-eval (eval (last exps) (interaction-environment)))
            (last-removed-exps (remove-last exps)))
        (cons (eval (first-operand exps) env)
              (list-of-values last-removed-exps env)))))
