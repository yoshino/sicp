(load "./machine_info")

;;----------------------------------------------------------
;;フィボナッチ
;;----------------------------------------------------------
(define fib-machine
  (make-machine
    '(n continue val)                            ;; register
     (list (list '+ +) (list '< <) (list '- -))   ;; operations
    '(controller
        (assign continue (label fib-done))
      fib-loop
        (test (op <) (reg n) ( const 2))
        (branch (label immediate-answer))   ;; Fib(n − 1) を求める準備
        (save continue)
        (assign continue (label afterfib-n-1))
        (save n)                            ;; n の古い値を保存
        (assign n (op -) (reg n) (const 1)) ;; n を n-1 で上書き
        (goto (label fib-loop))             ;; 再帰呼び出しの実行
      afterfib-n-1                          ;; リターン時に Fib(n − 1) は val に入っている
        (restore n)
        (restore continue)                  ;; Fib(n − 2) を求める準備
        (assign n (op -) (reg n) (const 2))
        (save continue)                     ;;<= ここを削除
        (assign continue (label afterfib-n-2))
        (save val)                          ;; Fib(n − 1) を保存
        (goto (label fib-loop))
      afterfib-n-2                          ;; リターン時に Fib(n − 2) は val に入っている
        (assign n (reg val))                ;; n には Fib(n − 2) が入る
        (restore val)                       ;; val には Fib(n − 1) が入る
        (restore continue)
        (assign val                         ;; Fib(n − 1) + Fib(n − 2)
          (op +) (reg val) (reg n))
        (goto (reg continue ))              ;; 呼び出し元に戻る、答えはval の中
      immediate-answer
        (assign val (reg n))                ;;基底の場合: Fib(n) = n
        (goto (reg continue))
      fib-done)))

(set-register-contents! fib-machine 'n 5)
(start fib-machine)
(get-register-contents fib-machine 'val)

;; 全命令のリスト
(define o (orders fib-machine))
(define order-types (map car (map car o)))
