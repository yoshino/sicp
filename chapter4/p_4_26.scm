;unless
;(unless (condition) (main) (alt))
;(unless (= b 0) (/ a b) (begin (display " exception : returning 0") 0))

(define (analyze-unless exp)
  (let ((condition (car (cdr e)))
        (main (cadr (cdr e)))
        (alt (caddr (cdr e))))
    (if (eval condition (interaction-environment))
        alt
        main)))

(define a 5)
(define b 10)
(define e '(unless (= b 0) (/ a b) (begin (display " exception : returning 0") 0)))
(define unless-res (analyze-unless e))
;=>(/ a b)
;遅延評価が上手く行っている
;gosh> (eval (analyze-unless e) (interaction-environment))
;1/2

; 特殊形式ではなく手続きとして便利な場合
; 以下のような文脈でconditionにunlessを渡す
(define (test args condition)
  (conditon (> args 0)
            #t
            #f))


