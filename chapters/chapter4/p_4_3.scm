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
     (apply (get (operator exp) env)
            (list-of-values (operands exp) env)))

    (else
      (error " Unknown expression type: EVAL" exp ))))

; dispatchを利用して定義する: データ主導
; 動的に型を追加できる

; 2.73より
(define (deriv exp var)
  (cond ((number? exp) 0)
               ((variable? exp) (if (same-variable? exp var) 1 0))
               (else ((get 'deriv (operator exp))
                      (operands exp) var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; 解答
(define (eval env)
    (define (dispatch exp)
      (if (application? exp)
          (apply (eval (operator exp) env)
                 (list-of-values (operands exp) env))
          (get exp)))
    dispatch)


; 正解参照
; self-evaluating?, variable?は事前に実行する必要がある
; self-evaluating?はstringか数字かを確認する
; variable?は変数(symbol)であるかどうかと、それが定義されているかを確認
(define (evaln expr env)
        (cond ((self-evaluating? expr) expr)
              ((variable? expr) (lookup-variable-value expr env))
              ((get 'op (car expr)) (applyn (get 'op (car expr) expr env))) ;(get 'op (car expr))で#t or #fが返ってくる
              ((application? expr)
               (applyn (evaln (operator expr) env)
                              (list-of-values (operands expr) env)))
              (else (error "Unknown expression type -- EVAL" expr))))

(define operation-table make-table)
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc))
(put 'op 'quote text-of-quotation)
(put 'op 'set! eval-assignment)
(put 'op 'define eval-definition)
(put 'op 'if eval-if)
(put 'op 'lambda (lambda (x y)
                   (make-procedure (lambda-parameters x) (lambda-body x) y)))
(put 'op 'begin (lambda (x y)
                  (eval-sequence (begin-sequence x) y)))
(put 'op 'cond (lambda (x y)
                  (evaln (cond->if x) y)))
(define (evaln expr env)
        (cond ((self-evaluating? expr) expr)
              ((variable? expr) (lookup-variable-value expr env))
              ((get 'op (car expr)) (applyn (get 'op (car expr) expr env)))
              ((application? expr)
               (applyn (evaln (operator expr) env)
                              (list-of-values (operands expr) env)))
              (else (error "Unknown expression type -- EVAL" expr))))
