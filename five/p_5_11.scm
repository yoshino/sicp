(load "./machine")
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

;(load "./p_5_11.scm")
;(set-register-contents! fib-machine 'n 5)
;(start fib-machine)
;gosh> done
;(get-register-contents fib-machine 'val)
;gosh> #t
;gosh>5

;a
;--------------------------------------------
; 上記の<=の行を削除できる
(define fib-machine2
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
        ;;(save continue)                     ;;<= ここを削除
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

;b
;--------------------------------------------
;以下をテスト
;これは失敗
(define test-machine1
  (make-machine
    '(a b)
    '()
    '(start
       (assign a (const 1))
       (assign b (const 2))
       (save a)
       (save b)
       (restore a)
       (restore b)
       (goto (label done))
      done)))

;これは成功
(define test-machine2
  (make-machine
    '(a b)
    '()
    '(start
       (assign a (const 1))
       (assign b (const 2))
       (save a)
       (save b)
       (restore b)
       (restore a)
       (goto (label done))
      done)))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    (lambda ()
      (let ((val (pop stack)))
       (cond ((eq? reg (car val))
       (set-contents! reg (cdr val))
       (advance-pc pc))
       (else
        (error "RESTORE require the same register as save, but" reg)))))))

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
   (lambda ()
     (push stack (cons reg (get-contents reg))) ;※ regも一緒にcons
     (advance-pc pc))))
