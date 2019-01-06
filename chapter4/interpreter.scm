; ---------------------------------------------------------------------
; 4.1.1: 評価機のコア
; ---------------------------------------------------------------------
; 世界の始まり：それは世界をどう見るかということ
; 以下のeval式の欠点は新しい型を追加したい場合にeval式にコードを追加しなければいけないこと
; 全てevalを経由するのでここが不完全だと実行できない
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
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
        ((application? exp)

         ; apply
         ; 手続き、引数のリストを引数にとる
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
          (error " Unknown expression type: EVAL" exp))))

; apply
(define (apply procedure arguments)
  (cond
    ; 基本演算の場合
    ((primitive-procedure? procedure)
     (apply-primitive-procedure procedure arguments))
    ; 複合手続きの場合
    ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error
            "Unknown procedure type: APPLY" procedure))))

; 手続きの引数
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; 条件文
; どちらの式をevalするかを決める
(define ( eval-if exp env)
  (if (true? (eval (if-predicate exp) env)) ; 被実装言語と実装言語(scheme)との境界面
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

; 列
; beginや複合手続きの中で使われる
(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps) env))
        (else
          (eval (first-exp exps) env)
          (eval-sequence (rest-exps exps) env))))

; 代入と定義
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)

; ---------------------------------------------------------------------
; 4.1.2: 式の表現
; ---------------------------------------------------------------------
; numberとstringが構成要素
(define ( self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else #f)))

(define (variable? exp) (symbol? exp))

; クォート
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      #f))

; 代入
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; 定義
; 手続きの定義とはlambda式に名前をつけたものに過ぎない
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp) ; 手続きの名前
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; 仮引数
                   (cddr exp)))) ; 本体

; lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; 条件式
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not ( null? ( cdddr exp)))
      (cadddr exp)
      'false)) ; falseはシンボル
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

; 列を単一の式に評価する
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

; その他：基礎的なセレクタといっていいのかもしれない
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; cond のような、構文変形として実装するようにした式のことは派生式という
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
        (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))
(define (expand-clauses clauses)
         (if (null? clauses)
             'false
             ; else 節はない
             (let ((first (car clauses))
                   (rest (cdr clauses)))
               (if (cond-else-clause? first)
                   (if (null? rest)
                       (sequence->exp (cond-actions first))
                       (error "ELSE clause isn't last: COND->IF "
                              clauses ))
                   (make-if (cond-predicate first) ; true? or false?
                             (sequence->exp (cond-actions first)) ; if true
                             (expand-clauses rest))))))           ; if false
