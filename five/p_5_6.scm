;練習問題 5.6: Ben Bitdiddle は、フィボナッチマシンのコントロー
;ラ命令列に余分な save と restore があり、取り除くことによって
;マシンを高速化できることに気がついた。それらの命令はどれか。

; 下記のHERE!部分
; restoreして何もしないでsaveしている

;フィボナッチ
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(controller
  (assign continue (label fib-done))
fib-loop
  (test (op <) (reg n) (const 2))
  (branch (label immediate-answer))
  (save continue)
  (assign continue (label afterfib-n-1))
  (save n)
  (assign n (op -) (reg n) (const 1))
  (goto (label fib-loop))
afterfib-n-1
  (restore n)
  (restore continue) ;;<= HERE!
  (assign n (op -) (reg n) (const 2))
  (save continue)    ;;<= HERE!
  (assign continue (label afterfib-n-2))
  (save val)
  (goto (label fib-loop))
afterfib-n-2
  (assign n (reg val))
  (restore val)
  (restore continue)
  (assign val (op +) (reg val) (reg n))
  (goto (reg continue ))
immediate-answer
  (assign val (reg n))
  ; 基底の場合: Fib(n) = n
  (goto (reg continue ))
fib-done)
