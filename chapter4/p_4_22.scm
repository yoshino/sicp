(define (tagged-list? exp tag)
 (if (pair? exp)
     (eq? (car exp) tag)
     #f))

(define (let? exp)
  (tagged-list? exp 'let))

(define (analyze-let exp)
  (define hash (car (cdr exp)))
  (define func (cadr (cdr exp)))
  (define vals (map car (map cdr hash)))
  (define keys (map car hash))
  (cons (list 'lambda keys func) vals))

(define e '(let ((one 1) (two 2)) (+ one two)))
(define let-res (analyze-let e))
; ((lambda (one two) (+ one two)) 1 2)

; 解析と実行を分ける
(define (aeval exp)
  (eval (analyze exp) (interaction-environment)))

(define (analyze exp)
  (cond ((let? exp) (analyze-let exp))))
