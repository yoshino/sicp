; 以下のように定義すると
;(define (cons x y) ( lambda (m) (m x y)))
;(define (car z) (z ( lambda (p q) p)))
;(define (cdr z) (z ( lambda (p q) q)))

; エラーが出てしまう
;;;; M-Eval input:
;(car '(a b c))
;*** ERROR: Unknown procedure type -- APPLY (quote (a b c))
;Stack Trace:
;_______________________________________
;  0  (eval input the-global-environment)
;        at "./driver-loop_stream.scm":11
;  1  (eval expr env)
;        at "/usr/share/gauche-0.9/0.9.5/lib/gauche/interactive.scm":282

;こういう形であれば
;gosh> (define l1 '(a b))
;gosh> (define l2 '(c d))
;gosh> (define res (cons l1 l2))
;#<closure ((cons cons) m)>
;gosh> (car res)
;(a b)
;gosh> (cdr res)
;(c d)

;(define (text-of-quotation exp) (cadr exp))

(define e '(a b c))

;gosh> (stream-bind e)
;(cons 'a (cons 'b (cons 'c ())))

(define (text-of-quotation exp env)
  (eval (stream-bind (cadr exp) env)))

(define (stream-bind l)
  (if (null? l)
      '()
      (list 'cons (list 'quote (car l) (stream-bind (cdr l))))))

(define (stream-bind l)
  (if (null? l)
      '()
      (list 'cons
            (list 'quote (car l)) (stream-bind (cdr l)))))

(define e '(a b c))
(define will-evaluate (stream-bind e))

; ここからストリーム化
; 本当はインタープリタ内に定義したいけど、car自体を定義に使っているのでできない。
; 結局、名前がかぶっていることがネックになっているように思う。
(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))

(define evaluated (eval will-evaluate (interaction-environment)))


