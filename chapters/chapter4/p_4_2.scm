(define (eval exp env)
  (cond
    ; 代入
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp))
    ((assignment? exp) (eval-assignment exp env))
    ((definition? exp) (eval-definition exp env))
    ((if? exp) (eval-if exp env))
    ((lambda? exp) (make-procedure (lambda-parameters exp)
                                   (lambda-body exp)
                                   env))
    ((begin? exp)
     (eval-sequence (begin-actions exp) env))
    ((cond? exp) (eval (cond->if exp) env))

    ; 手続きの適用
    ((application? exp) ; pairかどうか？
     (apply (eval (operator exp) env)
            (list-of-values (operands exp) env)))

    (else
      (error " Unknown expression type: EVAL" exp ))))

; 代入と手続きの適用を入れ替える
(define (eval exp env)
  (cond
    ; 手続きの適用
    ((application? exp)
     (apply (eval (operator exp) env)
            (list-of-values (operands exp) env)))

    ; 代入
    ((self-evaluating? exp) exp)
    ((variable? exp) (lookup-variable-value exp env))
    ((quoted? exp) (text-of-quotation exp))
    ((assignment? exp) (eval-assignment exp env))
    ((definition? exp) (eval-definition exp env))
    ((if? exp) (eval-if exp env))
    ((lambda? exp) (make-procedure (lambda-parameters exp)
                                   (lambda-body exp)
                                   env))
    ((begin? exp)
     (eval-sequence (begin-actions exp) env))
    ((cond? exp) (eval (cond->if exp) env))

    (else
      (error " Unknown expression type: EVAL" exp ))))

; 以下の場合を考える
; すると、定義されていないdefineがevalされてしまう。
(define x 3)

; 手続き適用がcallで始まるようにして、この問題を回避する

(define (application? exp) (pair? exp))
; after
(define (application? exp)
  (and (pair? exp) (tagged-list? exp 'call)))

(define (operator exp) (car exp))
; after
(define (operator exp) (cadr exp))

(define (operands exp) (cdr exp))
; after
(define (operands exp) (cddr exp))
